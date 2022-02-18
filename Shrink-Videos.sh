#!/bin/bash


# Constants
FFMPEG_DISPLAY_OPTIONS='-y -hide_banner -loglevel fatal -nostats'
FFMPEG_ENCODER_OPTIONS='-c:v libx265 -x265-params log-level=none -b:v 0'
VIDEO_SHRINKING_STRUCT='Shrink-Videos-Buffer.XXXX.mkv'
VIDEO_EXTENTIONS_TABLE='webm|mkv|flv|vob|ogv|ogg|rrc|gifv|mng|mov|avi|qt|wmv|yuv|rm|asf|amv|mp4|m4p|m4v|mpg|mp2|mpeg|mpe|mpv|m4v|svi|3gp|3g2|mxf|roq|nsv|flv|f4v|f4p|f4a|f4b'

# Command line argument derived variables
PROVIDED_VERBIAGE=3  # Default to verbosity, set to '5' for DEBUG output
PROVIDED_FILEPATH='.' # Default to current working directory


# General purpose function for standardized output
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


# Process the command line arguments
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


enumerate() {
    report 'tech' "Entering function call: 'enumerate'"

    local result=$1

    local file_extensions=$(echo "$VIDEO_EXTENTIONS_TABLE" | sed -e 's/|/\\|/g')
    local find_file_prune=$(echo "-type f -iregex '.*\.\(${file_extensions}\)\$' -exec")
    local ffprobe_options='-v quiet -show_streams -show_format -of json {} ;'

    report 'tech' "Extensions expanded: $file_extensions"
    report 'tech' "Find file pruning:   $find_file_prune"

    local json=$(find "${PROVIDED_FILEPATH}" \
                      -type f \
                      -iregex ".*\.\(${file_extensions}\)\$" \
                      -exec ffprobe $ffprobe_options)

#    report 'tech' "JSON:\n$json"

    local file_info_pairs=$(jq -c -r '.format.filename as $path | .format.size as $size | .streams[]? | select(.codec_type=="video" and .codec_name!="hevc") | [$size, $path] | @tsv' <<<"$json")

    #    report 'tech' "Extracted: $file_info_pairs"

    local descending_size=$(sort -hru <<<"$file_info_pairs")

#    report 'tech' "Sorted: $descending_size"

    local filepaths_alone=$(cut -d$'\t' -f 2 <<<"$descending_size")
    
#    report 'tech' "Trimmed: $filepaths_alone"

    local filepaths_dense=$(sed '/^$/d' <<<"$filepaths_alone")
    
#    report 'tech' "Trimmed: $filepaths_dense"

#    local filepaths_final=$(sed '$a\' <<<"$filepaths_alone")
    
#    report 'tech' "Trimmed: $filepaths_alone"

#    local quotes_escaping=$(sed 's/\x27/\\\x27/g' <<<"$filepaths_alone")
    
#    report 'tech' "Escaped: $quotes_escaping"
    
#    eval $result="\"$quotes_escaping\""
#    eval $result="\"$filepaths_final\n\""
    eval $result="\"$filepaths_dense\""
}


recode() {
    report 'tech' "Entering function call: 'recode'"
    
    local counter_prefix="$1"
    local video_filepath="$2"
    local video_filename=$(basename "$video_filepath")
    local buffer_address="$3"

    report 'info' "${counter_prefix}:\t$video_filename"

    local original_bytes=$(du    "$video_filepath" | cut -d$'\t' -f 1)
    local original_human=$(du -h "$video_filepath" | cut -d$'\t' -f 1)
    local initiation=$(date +%s)

    ffmpeg $FFMPEG_DISPLAY_OPTIONS -i "$video_filepath" $FFMPEG_ENCODER_OPTIONS "$buffer_address" &> /dev/null;
    status=$?

    local completion=$(date +%s)

    if [ $status -ne 0 ]; then
        report 'warn' "Error \t$video_filename"
        return 1;
    fi

    report 'tech' 'Statistics:\n'
    local recoding_phase=$(timespanned $initiation $completion)
    local shrunken_bytes=$(du    "$buffer_address" | cut -d$'\t' -f 1)
    local shrunken_human=$(du -h "$buffer_address" | cut -d$'\t' -f 1)
    local squashed_bytes=$((original_bytes-shrunken_bytes))
    report 'tech' "\toriginal_bytes:\t$original_bytes"
    report 'tech' "\toriginal_human:\t$original_human"
    report 'tech' "\tshrunken_bytes:\t$shrunken_bytes"
    report 'tech' "\tshrunken_human:\t$shrunken_human"
    report 'tech' "\tsquashed_bytes:\t$squashed_bytes"
    local size_reduction=$(bc -l <<<"(100*$squashed_bytes)/$original_bytes")
    report 'tech' "\tsize_reduction:\t$size_reduction"
    local nicely_rounded=$(printf %.2f%% $size_reduction)
    report 'tech' "\tnicely_rounded:\t$nicely_rounded"
    local display_result=$(printf "\t%s    %s / %s    %s\n" "$recoding_phase" "$shrunken_human" "$original_human" "$nicely_rounded")
    report 'tech' "\tdisplay_result:\t$display_result"


    report 'tech' 'Moving buffer to:'
    report 'tech' "${video_filepath%.*}.mkv"
    report 'tech' "mv $buffer_address ${video_filepath%.*}.mkv"

    rm -f "$video_filepath"
    mv "$buffer_address" "${video_filepath%.*}.mkv"

    report 'info' "$display_result"
    return 0;
}


# Display time in a standardized format
timespanned() {
    local B=$1
    local E=$2
    local T=$((E-B))
    local D=$((T/60/60/24))
    local H=$((T/60/60%24))
    local M=$((T/60%60))
    local S=$((T%60))
    printf '%02dd %02dh %02dm %02ds\n' $D $H $M $S
}


# Create a temporary workspace
setup() {
    report 'tech' "Entering function call: 'setup'"

    local result=$1
    local buffer=$(mktemp -t $VIDEO_SHRINKING_STRUCT)
    touch $buffer
    
    report 'tech' "Buffer file created: $buffer"
    
    eval $result="\"$buffer\""
}


# Remove temporary workspace
cleanup () {
    report 'tech' "Entering function call: 'cleanup'"

    local buffer=$(eval "echo \$$1")
    rm -rf "$buffer"
    report 'tech' "Buffer file removed: $buffer"
}


# 1st:
# Parse and process the commandline arguments.
parse "$@"


# 2nd:
# Determine the whther or not subscription information is available.
enumerate VIDEO_SHRINKING_INPUTS
          VIDEO_BYTES_REMOVED=0


# 4th:
# Precompute pretty printing variables
RECODE_OVERALL=$(   wc -l <<<"$VIDEO_SHRINKING_INPUTS")
RECODE_MAXIMUM=$(($(wc -c <<<"$RECODE_OVERALL") - 1))
RECODE_CURRENT=0
RECODE_SUCCESS=0

# 5th:
# Setup a temporary workspace.
setup VIDEO_SHRINKING_BUFFER


# 6th:
report 'info' "Re-encoding $RECODE_OVERALL video files...\n"
while read -r -u 9 FILEPATH || [ -n "$FILEPATH" ];
do
    ((RECODE_CURRENT+=1))
    COUNTER_PREFIX=$(printf "%${RECODE_MAXIMUM}d/%d" $RECODE_CURRENT $RECODE_OVERALL)
    recode "$COUNTER_PREFIX" "$FILEPATH" "$VIDEO_SHRINKING_BUFFER"
    status=$?
    if [ $status -eq 0 ]; then
        ((RECODE_SUCCESS+=1))         
    fi
     
done 9<<< "$VIDEO_SHRINKING_INPUTS";
report 'info' "Successfully recoded $RECODE_SUCCESS/$RECODE_OVERALL videos!"


# 8th:
# Remove temporarily allocated resources
cleanup VIDEO_SHRINKING_BUFFER
report 'loud' 'Finished re-encoding video files!'

