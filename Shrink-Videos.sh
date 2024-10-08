#!/bin/bash
trap "cleanup VIDEO_SHRINKING_BUFFER" ERR EXIT INT QUIT TERM


# Command line argument derived variables.
PROVIDED_FILEPATH='.' # Default to current working directory
PROVIDED_VERBIAGE=3   # Default to moderate verbosity: [0,5] -> [QUIET, ERROR, WARNS, INFOS, EXTRA, DEBUG]


# General purpose function for standardized output.
report() {
  if [ $PROVIDED_VERBIAGE -le 0 ]; then return 0; fi
  local prefix=''
  case "$1" in
      tech) if [[ $PROVIDED_VERBIAGE -ge 5 ]]; then prefix='# '   ; else return 0; fi ;;
      loud) if [[ $PROVIDED_VERBIAGE -ge 4 ]]; then prefix='  '   ; else return 0; fi ;;
      warn) if [[ $PROVIDED_VERBIAGE -ge 2 ]]; then prefix='! '   ; else return 0; fi ;;
      fail) if [[ $PROVIDED_VERBIAGE -ge 1 ]]; then prefix='X '   ; else return 0; fi ;;
      *)    if [[ $PROVIDED_VERBIAGE -ge 3 ]]; then prefix='  '   ; else return 0; fi ;;
  esac
  echo -e "$prefix$2"
}


# Process the command line arguments.
parse() {
    local OPTIND
    while getopts ":xweq" opt; do
        case $opt in
            x) PROVIDED_VERBIAGE=4;;
            w) PROVIDED_VERBIAGE=2;;
            e) PROVIDED_VERBIAGE=1;;
            q) PROVIDED_VERBIAGE=0;;
            \?) report 'fail' "Invalid option -$OPTARG" >&2 && exit 1 ;;
        esac
    done

    shift "$((OPTIND - 1))"
    # Now "$@" contains the rest of the arguments

    # If there are one or more remaining command line arguments,
    # they are the source code filepaths!
    if [ "$#" -ne 0 ]; then
        PROVIDED_FILEPATH="$1"
    fi
    
    report 'tech' 'Collected parameters:'
    report 'tech' "PROVIDED_VERBIAGE:\t'$PROVIDED_VERBIAGE'"
    report 'tech' "PROVIDED_FILEPATH:\t'$PROVIDED_FILEPATH'"
}


# Gather all files on the specified path which can be re-encoded.
enumerate() {
    report 'tech' "Entering function call: 'enumerate'"

    local descending_size ffprobe_options file_extensions file_info_pairs \
          filepaths_alone filepaths_dense filepaths_final json video_exts_list
    
    video_exts_list='webm|mkv|flv|vob|ogv|ogg|rrc|gifv|mng|mov|avi|qt|wmv|yuv|rm|asf|amv|mp4|m4p|m4v|mpg|mp2|mpeg|mpe|mpv|m4v|svi|3gp|3g2|mxf|roq|nsv|flv|f4v|f4p|f4a|f4b'
    file_extensions="${video_exts_list//|/\\|}"
    ffprobe_options='-v quiet -show_streams -show_format -of json {} ;'

    report 'tech' "Extensions expanded: ${file_extensions}"

    json=$(find "${PROVIDED_FILEPATH}" \
        -type f \
        -iregex ".*\.\(${file_extensions}\)\$" \
        -exec ffprobe $ffprobe_options)

    file_info_pairs=$(jq -c -r '.format.filename as $path | .format.size as $size | .streams[]? | select(.codec_type=="video" and .codec_name!="hevc") | [$size, $path] | @tsv' <<<"$json")

    descending_size=$( ( sort -hru        ) <<<"$file_info_pairs") # Sort by 1st field (file size)
    filepaths_alone=$( ( cut -d$'\t' -f 2 ) <<<"$descending_size") # Extract 2nd field (file name)
    filepaths_dense=$( ( sed '/^$/d'      ) <<<"$filepaths_alone") # Remove any empty lines
    filepaths_final=$filepaths_dense

    report 'tech' "Found files:\n$filepaths_final"

    VIDEO_SHRINKING_INPUTS="${filepaths_final}"
    VIDEO_FILES_ENUMERATED='Done'
}


# Perform video re-encoding to a more compact format.
recode() {
    report 'tech' "Entering function call: 'recode'"
    
    local result=$1
    local byte_prune_sum="${2}"
    local buffer_address="${3}"
    local counter_prefix="${4}"
    local video_filepath="${5}"

    local completion initiation original_bytes original_human video_filename video_nicename 

    video_filename=$(basename "${video_filepath}")
    video_nicename=${video_filename%.*}

    original_human=$(du -h   "${video_filepath}" | cut -d$'\t' -f 1)
    original_bytes=$(du -B 1 "${video_filepath}" | cut -d$'\t' -f 1)
    initiation=$(date +%s)
    start_time=$(date '+%F %R')

    report 'loud' "${counter_prefix} @ ${start_time}:\t${video_nicename}"

    output=$(ffmpeg \
        -y -hide_banner -loglevel error -nostats \
        -i "${video_filepath}" \
        -b:v 0 \
        -c:v libx265 \
        -x265-params log-level=error \
        -crf 25 \
        -max_muxing_queue_size 4096 \
        -preset slower \
        -vf 'crop=trunc(iw/2)*2:trunc(ih/2)*2' \
        -f matroska \
        "${buffer_address}" 2>&1);
    status=$?
    completion=$(date +%s)

    if [ $status -ne 0 ]; then
        RECODE_VERDICT=${RECODE_FAILURE}
        report 'warn' "Skip: \t${video_filename}"
        report 'loud' "Error:\t${output}"
        return 0;
    fi

    local shrunken_human shrunken_bytes squashed_bytes
    shrunken_human=$(du -h   "${buffer_address}" | cut -d$'\t' -f 1)
    shrunken_bytes=$(du -B 1 "${buffer_address}" | cut -d$'\t' -f 1)
    squashed_bytes=$(( original_bytes-shrunken_bytes ))
                     
    if [ $PROVIDED_VERBIAGE -ge 4 ]; then
        local 
        report 'tech' 'Statistics:\n'
        report 'tech' "\toriginal_bytes:\t${original_bytes}"
        report 'tech' "\toriginal_human:\t${original_human}"
        report 'tech' "\tshrunken_bytes:\t${shrunken_bytes}"
        report 'tech' "\tshrunken_human:\t${shrunken_human}"
        report 'tech' "\tsquashed_bytes:\t${squashed_bytes}"
    fi

    # If the result is a smaller video file,
    # then we take the re-encoded result!
    if [ ${squashed_bytes} -ge 0 ]; then
        if [ $PROVIDED_VERBIAGE -ge 4 ]; then
            local counter_string display_result nicely_rounded recoding_phase size_reduction whitespace_pad
            counter_string="${counter_prefix}"
            whitespace_pad=$(printf "%*s" ${#counter_string} "")
            recoding_phase=$(timespanned "${initiation}" "${completion}")
            size_reduction=$(bc -l <<<"(100*${squashed_bytes})/${original_bytes}")
            nicely_rounded=$(printf %.2f%% "${size_reduction}")
            display_result=$(printf "%s\t%s    %s / %s    %s\n" "${whitespace_pad}" "${recoding_phase}" "${shrunken_human}" "${original_human}" "${nicely_rounded}")
            report 'loud' "${display_result}\n"
        fi
    
        report 'tech' 'Moving buffer to:'
        report 'tech' "${video_filepath%.*}.mkv"
        report 'tech' "mv ${buffer_address} ${video_filepath%.*}.mkv"

        rm -f "${video_filepath}"
        mv "${buffer_address}" "${video_filepath%.*}.mkv"
    
        (( byte_prune_sum+=squashed_bytes ))
        eval "${result}"="\"${byte_prune_sum}\""

    # Otherwise we copy the input into an MKV container.
    else
        if [ $PROVIDED_VERBIAGE -ge 4 ]; then
            local counter_string display_result nicely_rounded recoding_phase size_reduction whitespace_pad
            counter_string="${counter_prefix}"
            whitespace_pad=$(printf "%*s" ${#counter_string} "")
            recoding_phase=$(timespanned "${initiation}" "${completion}")
            size_reduction=$(bc -l <<<"(100*${squashed_bytes})/${original_bytes}")
            nicely_rounded=$(printf %.2f%% "${size_reduction}")
            display_result=$(printf "%s\t%s    [Increased]    (Copying)\n" "${whitespace_pad}" "${recoding_phase}" "${shrunken_human}" "${original_human}" "${nicely_rounded}")
            report 'loud' "${display_result}\n"
        fi
        report 'tech' 'Copying input stream into MKV to:'
        report 'tech' "${video_filepath%.*}.mkv"
        report 'tech' "ffmpeg -i ${video_filepath} ${video_filepath%.*}.mkv"

        output=$(ffmpeg \
            -y -hide_banner -loglevel error -nostats \
            -i "${video_filepath}" \
            -b:v 0 \
            -c copy \
            "${buffer_address}" 2>&1);
        status=$?
        rm -f "${video_filepath}"
        mv "${buffer_address}" "${video_filepath%.*}.mkv"

        eval "${result}"="\"${byte_prune_sum}\""
    fi

    RECODE_VERDICT=${RECODE_SUCCESS}
}


# Display time in a standardized format.
timespanned() {
    local B=$1
    local E=$2
    local T=$(( E-B ))
    local D=$(( T/60/60/24 ))
    local H=$(( T/60/60%24 ))
    local M=$(( T/60%60    ))
    local S=$(( T%60       ))
    printf '%02dd %02dh %02dm %02ds\n' $D $H $M $S
}


# Create a temporary workspace.
setup() {
    report 'tech' "Entering function call: 'setup'"

    local result=$1
    local buffer struct
    struct='Shrink-Videos-Buffer.XXXX.mkv'
    buffer=$(mktemp -t ${struct})
    touch "${buffer}"
    
    report 'tech' "Buffer file created: ${buffer}"

    eval "${result}"="\"${buffer}\""
}

summerize() {
    if [ $PROVIDED_VERBIAGE -ge 4 ]; then
        report 'loud' "Re-encoding $VIDEO_START_FILE_HUMAN in $RECODE_OVERALL video files:\n"
    else
        report 'info' "Re-encoding $RECODE_OVERALL video files totaling $VIDEO_START_FILE_HUMAN..."
    fi
    RECODE_STARTED=${RECODE_SUCCESS}
}


recountal() {
    # Check if we have started
    if [[ -z "${RECODE_STOPPED}" ]]; then
        # Add a newline to output *iff* a signal was sent to stop the process.
        if [[ ! -z "${SIGNAL_FLAG}" ]]; then
            report 'info' ""
        fi

        if [[ -n "${RECODE_STARTED}" ]] && [[ ${RECODE_STARTED} -eq ${RECODE_SUCCESS} ]]; then

            # Add a newline to output *iff*
            #   1. a newline was not added above because signal was sent to stop the process and
            #   2. the verbosity is set to 'LOUD'
            if [[ -z "${SIGNAL_FLAG}" ]]; then
                report 'loud' ""
            fi
            report 'info' "Successfully re-encoded ${RECODE_UPDATED}/${RECODE_OVERALL} videos!"

            if [ $PROVIDED_VERBIAGE -ge 4 ]; then
                VIDEO_PRUNE_FILE_HUMAN=$(numfmt --to=iec "${VIDEO_PRUNE_FILE_BYTES}")
                VIDEO_SMALL_FILE_BYTES=$(( VIDEO_START_FILE_BYTES-VIDEO_PRUNE_FILE_BYTES ))
                VIDEO_SMALL_FILE_HUMAN=$(numfmt --to=iec "${VIDEO_SMALL_FILE_BYTES}")
                VIDEO_RATIO_FILE_BYTES=$(bc -l <<<"(100*${VIDEO_PRUNE_FILE_BYTES})/${VIDEO_START_FILE_BYTES}")
                VIDEO_RATIO_FILE_HUMAN=$(printf %.2f%% "${VIDEO_RATIO_FILE_BYTES}")
                report 'loud' "Processing elapsed for a total duration of ${RECODE_RUNPERIOD}"
                report 'loud' "Re-encoding removed ${VIDEO_PRUNE_FILE_HUMAN}, reducing total file size by ${VIDEO_RATIO_FILE_HUMAN} ( ${VIDEO_SMALL_FILE_HUMAN} / ${VIDEO_START_FILE_HUMAN} )"
            fi
        elif [[ -n "${VIDEO_FILES_ENUMERATED}" ]]; then
            report 'info' "No re-encoding candidate video files found."
        else
            report 'loud' "Stopped before enumeration of files was complete."
        fi
   fi
   RECODE_STOPPED=0
}


# Remove temporary workspace.
cleanup () {
    code=$?
    if [[ $code -ne 0 ]]; then
        SIGNAL_FLAG=0
    fi

    report 'tech' "Entering function call: 'cleanup'"
    
    local buffer
    buffer=$(eval "echo \$$1")
    rm -rf "${buffer}"
    report 'tech' "Buffer file removed: ${buffer}"
    
    recountal

    exit $code
}


# 1st:
# Parse and process the commandline arguments.
parse "$@"

# 2nd:
# Determine the whether or not subscription information is available.
enumerate

# 3rd:
# Exit early if there were no files found.
if [[ -z "${VIDEO_SHRINKING_INPUTS}" ]]; then exit 0; fi

# 4th:
# Define re-encoding return result signals
RECODE_SUCCESS=0
RECODE_FAILURE=$(( ~(RECODE_SUCCESS) ))
RECODE_VERDICT=${RECODE_FAILURE}

# 5th:
# Precompute pretty printing variables.
RECODE_OVERALL=$(    wc -l <<<"${VIDEO_SHRINKING_INPUTS}")
RECODE_MAXIMUM=$(( $(wc -c <<<"${RECODE_OVERALL}") - 1 ))
RECODE_CURRENT=0
RECODE_UPDATED=0
RECODE_STARTED=${RECODE_FAILURE}

# 6th:
# Note starting file size statistics for subsequent reporting.
VIDEO_START_FILE_HUMAN=$( (xargs -d$'\n' du -c -h   | tail -n 1 | cut -d$'\t' -f1) <<<"${VIDEO_SHRINKING_INPUTS}")
VIDEO_START_FILE_BYTES=$( (xargs -d$'\n' du -c -B 1 | tail -n 1 | cut -d$'\t' -f1) <<<"${VIDEO_SHRINKING_INPUTS}")
VIDEO_PRUNE_FILE_BYTES=0

# 7th:
# Setup a temporary workspace.
setup VIDEO_SHRINKING_BUFFER

# 8th:
# Summerize the total re-encoding candidate videos which were found.
summerize

# 9th:
# Re-encode all applicable video files which were located.
RECODE_BEGINNING=$(date +%s)
while read -r -u 9 FILEPATH || [ -n "$FILEPATH" ];
do
    (( RECODE_CURRENT+=1 ))
    RECODE_VERDICT=${RECODE_FAILURE} # Assume recoding failure
    COUNTER_PREFIX=$(printf "%${RECODE_MAXIMUM}d/%d" "${RECODE_CURRENT}" "${RECODE_OVERALL}")
    recode VIDEO_PRUNE_FILE_BYTES "${VIDEO_PRUNE_FILE_BYTES}" "${VIDEO_SHRINKING_BUFFER}" "${COUNTER_PREFIX}" "${FILEPATH}"
    if [ "${RECODE_VERDICT}" -eq "${RECODE_SUCCESS}" ]; then
        (( RECODE_UPDATED+=1 ))
    fi
     
done 9<<<"${VIDEO_SHRINKING_INPUTS}";
RECODE_CONCLUDED=$(date +%s)
RECODE_RUNPERIOD=$(timespanned "${RECODE_BEGINNING}" "${RECODE_CONCLUDED}")
