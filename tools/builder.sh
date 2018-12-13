#!/bin/bash -e
#
# Abstract helper for building CaRMa.
#
# WARNING! Unicode symbols are used in this script,
#          so make sure you have UTF-8 in your $LANG.
#

# Use -h or --help option to show it
show_usage() {
local s=$(printf '%s' "$0" | sed 's/./ /g')
cat << USAGE

Usage: $0 [-c|--clean] [--full-clean]
       $s [-p|--parallel] [--production] [--ci]
       $s TASKS…

Multiple tasks (task groups, runs other tasks inside):
    $0 all           Build everything
    $0 backend       Build backend
    $0 backend-test  Run all tests for backend
    $0 backend-docs  Build haddock documentation for backend
    $s               (tests that haddock syntax in modules is correct)
    $0 frontend      Build frontend
    $0 test          Run all tests
    $0 docs          Run all documentation generation tasks

Singular tasks:
    $0 backend-configs             Copy configs examples if needed
    $0 backend-carma               Just build CaRMa backend

    $0 backend-test-configurator   Run tests for configs
    $s                             handled by "carma-configurator" tool

    $0 backend-test-era-glonass-integration
    $s                             Run tests for Era Glonass
    $s                             integration microservice

    $0 backend-docs-era-glonass-integration
    $s                             Build haddock documentation for Era Glonass
    $s                             integration microservice

    $0 frontend-pure               Build "pure" frontend
    $0 frontend-legacy             Build "legacy" frontend
    $0 frontend-backend-templates  Build templates which used by backend

Options:
    -c, --clean     Clean previous built executables/bundles before build

    --full-clean    Apart from regular cleaning also removes directories like
                    .stack-work for backend or node_modules for frontend.

                    P.S. When this flag is set you don't have to set --clean.

    -p, --parallel  To run tasks in parallel

    -b, --bare-app-log

                    Disable adding label before each line of an application's
                    log which shows task name, current application/command that
                    log message belongs to, marks separately STDOUT/STDERR.

                    When you don't run tasks in parallel labels for each line of
                    an application's log could be just noisy since they doesn't
                    bring anything useful in this case. For case of parallelism
                    they allow you to separate log messages of one application
                    from another.

    --production    Make a build for production (minify, no debug stuff, etc.)

    --ci            Marking that build happens inside CI container
                    (see .circleci/config.yml)

USAGE
}

# A helper that checks if a value is an element of an array.
#
# Usage example:
#   if elem "some-value" "${some_array[@]}"; then
#       echo "yes"
#   else
#       echo "no"
#   fi
#
elem() {
    local value=$1
    shift
    for it in "$@"; do
        [[ $it == $value ]] && return 0
    done
    return 1
}

# Working directory is CaRMa's root
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

positional=() # TASKS
positional_split=false
run_in_parallel=false
is_production_build=false
is_ci_container=false
is_clean_build=false
is_fully_clean_build=false
is_bare_app_log=false

available_tasks=(
    all test docs
    backend backend-configs backend-carma backend-test backend-docs
    backend-test-configurator backend-test-era-glonass-integration
    backend-docs-era-glonass-integration
    frontend frontend-pure frontend-legacy frontend-backend-templates
)

for arg in "$@"; do
    if [[ $positional_split == true ]]; then
        if elem "$arg" "${available_tasks[@]}"; then
            positional+=("$arg")
        else
            printf 'Unknown task: "%s"\n' "$arg" >&2
            show_usage >&2
            exit 1
        fi
    else
        case $arg in
            --)
                positional_split=true
                ;;
            --production)
                is_production_build=true
                ;;
            --ci)
                is_ci_container=true
                ;;
            -c|--clean)
                is_clean_build=true
                ;;
            --full-clean)
                is_clean_build=true
                is_fully_clean_build=true
                ;;
            -p|--parallel)
                run_in_parallel=true
                ;;
            -b|--bare-app-log)
                is_bare_app_log=true
                ;;
            -h|--help|help)
                show_usage
                exit 0
                ;;
            -*)
                printf 'Unknown option: "%s"!\n' "$arg" >&2
                show_usage >&2
                exit 1
                ;;
            *)
                if elem "$arg" "${available_tasks[@]}"; then
                    positional+=("$arg")
                else
                    printf 'Unknown task: "%s"\n' "$arg" >&2
                    show_usage >&2
                    exit 1
                fi
        esac
    fi
done

if (( ${#positional[*]} == 0 )); then
    echo 'No task specified!' >&2
    show_usage >&2
    exit 1
fi


mk_tmp_fifo() {
    local fifo=$(mktemp -u)
    mkfifo -- "$fifo"
    printf '%s\n' "$fifo"
}

stdout_fifo=$(mk_tmp_fifo)
stderr_fifo=$(mk_tmp_fifo)
logger_pids=()

# General centralized logger.
# All tasks supposed to write their logs to FIFOs handled in this function.
# $1 - "1" or "2", means either stdout or stderr logger.
logger() {
    local f=

    if (( $1 == 1 )); then
        f=$stdout_fifo
    else
        f=$stderr_fifo
        exec >&2
    fi

    cat "$f" | while IFS= read -r x; do
        printf "%s\n" "$x"
    done
}

logger 1 & logger_pids+=($!)
logger 2 & logger_pids+=($!)
exec 3>"$stdout_fifo" 4>"$stderr_fifo"

exit_hook() {
    local rv=$?

    # Cleanup

    exec 3>&- 4>&-
    for pid in "${logger_pids[@]}"; do wait -- "$pid"; done

    # Writing at the end of log
    if (( $rv != 0 )); then
        printf '%s: Application is failed with status: %d%s\n' \
            "$0" "$rv" ' (probably because some task is failed)!' >&2
    fi

    rm -- "$stdout_fifo" "$stderr_fifo" "$tasks_counter"
}

trap exit_hook EXIT


# Pre-cached "tput" command results (for optimization purposes).
# Doesn't color when run with --ci ($TERM is not set there, "tput" is failing).
if [[ $is_ci_container != true ]]; then
    c_black=$(tput setaf 0)
    c_red=$(tput setaf 1)
    c_green=$(tput setaf 2)
    c_yellow=$(tput setaf 3)
    c_blue=$(tput setaf 4)
    c_magenta=$(tput setaf 5)
    c_cyan=$(tput setaf 6)
    c_white=$(tput setaf 7)
    c_bold=$(tput bold)
    c_reset=$(tput sgr0)
fi

tasks_counter=$(mktemp)
echo 0 >"$tasks_counter"

# $1 is task name.
# [[ $2 == run ]] means task is running
# [[ $2 == done ]] means task is done
# [[ $2 == fail ]] means task is failed
# [[ $2 == step ]] to log particular sub-tasks
# [[ $2 == app-stdout ]] to log application's messsage
# [[ $2 == app-stderr ]] to log application's error message
# $3 is log message when [[ $2 == step ]]
# $3 is exit status when [[ $2 == failed ]]
# $4 is application name when [[ $2 == app-stdout ]] || [[ $2 == app-stderr ]]
task_log() {
    (
    local task_name=$1
    local task_name_c=${c_cyan}${task_name}${c_reset}
    local d=$(date '+%Y-%m-%d %H:%M:%S')
    local d_c=${c_blue}${d}${c_reset}

    if [[ $2 == run ]] || [[ $2 == done ]] || [[ $2 == fail ]]; then
        local pfx=$(printf '[%s] "%s" task is ' "$d_c" "$task_name_c") sfx=

        if [[ $2 == run ]]; then
            pfx=$(printf '%s%srunning%s' \
                "$pfx" "${c_bold}${c_magenta}" "${c_reset}")

            local c=$[`
                flock --exclusive -- "$tasks_counter" \
                    perl -p -i -e 'print STDERR;$_++' -- "$tasks_counter" 2>&1`]
            if (( $c > 0 )); then
                sfx=" (${c_bold}${c_magenta}${c}${c_reset}"
                sfx="${sfx} other task(s) is active)"
            fi

            sfx="${sfx}…"
        else # [[ $2 == done ]] || [[ $2 == fail ]]
            if [[ $2 == done ]]; then
                pfx=$(printf '%s%sdone%s' \
                    "$pfx" "${c_bold}${c_green}" "${c_reset}")
            else
                pfx=$(printf '%s%sfailed%s with exit status %s' \
                    "$pfx" "${c_bold}${c_red}" "${c_reset}" \
                    "${c_bold}${c_red}${3}${c_reset}")
            fi

            local c=$[`
                flock --exclusive -- "$tasks_counter" \
                    perl -p -i -e '$_--;print STDERR' -- "$tasks_counter" 2>&1`]
            if (( $c > 0 )); then
                sfx=" (${c_bold}${c_magenta}${c}${c_reset}"
                sfx="${sfx} other task(s) is still active)"
            fi

            sfx="${sfx}."
        fi

        if [[ $2 == fail ]]
            then printf '%s%s\n' "$pfx" "$sfx" >&2
            else printf '%s%s\n' "$pfx" "$sfx"; fi

    elif [[ $2 == step ]]; then
        printf '[%s] "%s" task: %s\n' "$d_c" "$task_name_c" \
            "${c_yellow}${3}${c_reset}"

    elif [[ $2 == app-stdout ]] || [[ $2 == app-stderr ]]; then
        if [[ $is_bare_app_log == true ]]; then
            if [[ $2 == app-stderr ]]
                then printf '%s\n' "$3" >&2
                else printf '%s\n' "$3"; fi
            return 0
        fi

        local std=$([[ $2 == app-stdout ]] && echo STDOUT || echo STDERR)
        local sep= app_name=$4
        local app_name_c=${c_cyan}${app_name}${c_reset}

        local pfx=$(
            printf '[%s] "%s" task "%s" app [%s]: ' \
                "$d" "$task_name" "$app_name" "$std")

        (( ${#pfx} > 40 )) && sep=$'\n  '$"${c_bold}${c_yellow}↪${c_reset} "

        local std_c=$(
            [[ $2 == app-stdout ]] \
                && printf '%s' "${c_bold}${c_green}${std}${c_reset}" \
                || printf '%s' "${c_bold}${c_red}${std}${c_reset}")

        pfx=$(
            printf '[%s] "%s" task "%s" app [%s]: ' \
                "$d_c" "$task_name_c" "$app_name_c" "$std_c")

        if [[ $2 == app-stderr ]]
            then printf '%s%s%s%s\n' "$pfx" "$sep" "$3" "${c_reset}" >&2
            else printf '%s%s%s%s\n' "$pfx" "$sep" "$3" "${c_reset}"; fi

    else
        printf '[%s] Unexpected "%s" task "%s" action!\n' \
            "$d_c" "$task_name_c" "${c_red}${2}${c_reset}" >&2
        exit 1
    fi
    ) 1>&3 2>&4
}

# Log end of task and fail if needed.
# $1 - task name
# $2 - exit status
task_resolve() {
    if (( $2 == 0 )); then
        task_log "$1" done
    else
        task_log "$1" fail "$2"
        return -- "$2"
    fi
}

# $1 - '1' for stdout and '2' for stderr
# $2 - FIFO file descriptor
# $3 - task name to log
# $4 - application name to log
#
# Usage example (usual general template used in the script):
#   local task_name='some-task-name' dir='srv'
#   …
#   local app_name='npm install'
#   local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
#   app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
#   app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
#   (cd -- "$dir" && npm install) \
#       1>"$lout" 2>"$lerr" \
#       || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
#       || return -- "$?"
#   for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
#   rm -- "$lout" "$lerr"
#
app_logger() {
    local action=`(( $1 == 1 )) && echo app-stdout || echo app-stderr`

    cat -- "$2" | while IFS= read -r x; do
        task_log "$3" "$action" "$x" "$4"
    done
}

# $1 - task name (leave empty if it's task helper to avoid task resolving)
# $2 - a string of PIDs separated by spaces (logger PIDs to wait for)
# $@ - FIFOs files to cleanup
fail_trap() {
    local rv=$? task_name=$1 pids=($2)
    shift
    shift
    for pid in "${pids[@]}"; do wait -- "$pid"; done
    (( $# > 0 )) && rm -- "$@"

    if [[ ! -z $task_name ]]; then
        task_resolve "$task_name" "$rv"
    else
        return -- "$rv"
    fi
}


# *** START OF TASKS SECTION ***


# "frontend-pure" task
frontend_pure_task__covered_by=(frontend all)
frontend_pure_task() {
    local task_name='frontend-pure' dir='srv/resources/assets/pure'
    task_log "$task_name" run

    # Full clean
    if [[ $is_fully_clean_build == true ]]; then
        task_log "$task_name" step $"Removing \"$dir/node_modules\" directory…"
        local app_name='rm -rf node_modules'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && rm -rf node_modules) \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    # Installing dependencies
    task_log "$task_name" step 'Installing dependencies…'
    local app_name='npm install'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm install) \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    # Fetching submodules for Circle CI container
    if [[ $is_ci_container == true ]]; then
        task_log "$task_name" step 'Cloning git submodules…'

        local app_name='purescript-react-dropzone git submodule'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-dropzone) \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"

        local app_name='purescript-react-rich-text-editor git submodule'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-rich-text-editor) \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    local ci_flags=()
    # For Circle CI container allowing installing packages by superuser
    [[ $is_ci_container == true ]] && ci_flags=('--allow-root')

    # Full clean
    if [[ $is_fully_clean_build == true ]]; then
        task_log "$task_name" step \
            $"Removing \"$dir/bower_components\" directory…"
        local app_name='rm -rf bower_components'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && rm -rf bower_components) \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    # Installing PureScript dependencies
    task_log "$task_name" step \
        'Installing PureScript dependencies (using bower)…'
    local app_name='bower install'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm run bower -- install "${ci_flags[@]}") \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    local clean_infix=`[[ $is_clean_build == true ]] && echo clean-`
    local prod_prefix=`[[ $is_production_build == true ]] && echo prod-`
    local npm_task=${prod_prefix}${clean_infix}build
    task_log "$task_name" step $"Running npm \"$npm_task\" script…"
    local app_name="npm run $npm_task"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# Preparation for legacy frontend build.
# Helps to make it for both "frontend-legacy" and "frontend-backend-templates"
# tasks when they're both specified, no need to do it twice.
frontend_legacy_pre() {
    local task_name='frontend-legacy[pre]' dir='srv'
    task_log "$task_name" run

    if [[ $is_fully_clean_build == true ]]; then
        task_log "$task_name" step $"Removing \"$dir/node_modules\" directory…"
        local app_name='rm -rf node_modules'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && rm -rf node_modules) \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    task_log "$task_name" step 'Installing dependencies…'
    local app_name='npm install'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm install) \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "frontend-legacy" task
frontend_legacy_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
frontend_legacy_task() {
    local task_name='frontend-legacy' dir='srv'
    task_log "$task_name" run

    [[ $1 == true ]] \
        || frontend_legacy_pre \
        || fail_trap "$task_name" '' \
        || return -- "$?"

    local clean_infix=
    [[ $is_clean_build == true ]] && clean_infix='clean-'

    local prod_prefix=
    [[ $is_production_build == true ]] && prod_prefix='prod-'

    local npm_task="${prod_prefix}${clean_infix}build"
    task_log "$task_name" step $"Running npm \"$npm_task\" script…"
    local app_name="npm run $npm_task"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "frontend-backend-templates" task
frontend_backend_templates_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
# It's part of a legacy frontend, so it's kinda connected to it
# (that's why they're both shared "frontend_legacy_pre").
frontend_backend_templates_task() {
    local task_name='frontend-backend-templates' dir='srv'
    task_log "$task_name" run

    [[ $1 == true ]] \
        || frontend_legacy_pre \
        || fail_trap "$task_name" \
        || return -- "$?"

    local clean_prefix=
    [[ $is_clean_build == true ]] && clean_prefix='clean-'

    local npm_task=${clean_prefix}build-backend-templates
    task_log "$task_name" step $"Running npm \"$npm_task\" script…"
    local app_name="npm run $npm_task"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# Helper to run legacy tasks in parallel
frontend_task_parallel_legacy() {
    frontend_legacy_pre
    local pids=() rv=0
    frontend_legacy_task true & pids+=($!)
    frontend_backend_templates_task true & pids+=($!)
    for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    return -- "$rv"
}

# "frontend" task
frontend_task__covered_by=(all)
frontend_task() {
    local task_name='frontend' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        frontend_pure_task & pids+=($!)
        frontend_task_parallel_legacy & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        frontend_pure_task
        frontend_legacy_pre
        frontend_legacy_task true
        frontend_backend_templates_task true
    fi
    task_resolve "$task_name" "$rv"
}


# A helper for copying example configs to real configs paths.
# $1 - task name for log
# $2 - label of a config
# $3 - config example file (as a source to copy from)
# $4 - destination of a config
config_copy() {
    task_log "$1" step $"Checking $2 config file: \"$4\"…"
    if [[ -f $4 ]]; then
        task_log "$1" step $"$2 config file \"$4\" exists."
    else
        local x=$"$2 config file \"$4\" doesn't exist,"
        x=$"$x copying from \"$3\" to \"$4\"…"
        task_log "$1" step "$x"

        local app_name="cp -- '$3' '$4'"
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$1" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$1" "$app_name" & pids+=($!)
        cp -- "$3" "$4" \
            1>"$lout" 2>"$lerr" \
            || fail_trap '' "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"

        task_log "$1" step $"$2 config file \"$4\" is copied."
    fi
}

# A helper for copying default snaplets configs to actual snaplets configs path.
# $1 - task name for log
# $2 - snaplets configs directory path
#      (default snaplets configs directory path
#       will be suffixed with `-default`)
snaplet_config_copy() {
    task_log "$1" step "Checking $2 configs directory…"
    if [[ ! -d $2 ]]; then
        local x="$2 configs directory doesn't exists, copying it"
        x="$x from ${2}-default to ${2}…"
        task_log "$1" step "$x"

        local app_name="cp -r -- '${2}-default' '$2'"
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$1" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$1" "$app_name" & pids+=($!)
        cp -r -- "${2}-default" "$2" \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$1" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"

        task_log "$1" step "$2 configs directory is copied."
    else
        local t="$2 configs directory exists,"
        t="$t checking particular configs…"
        task_log "$1" step "$t"

        ls -- "${2}-default" | while read dir; do
            local app_name="mkdir -p -- '$2/$dir'"
            local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
            app_logger 1 "$lout" "$1" "$app_name" & pids+=($!)
            app_logger 2 "$lerr" "$1" "$app_name" & pids+=($!)
            mkdir -p -- "$2/$dir" \
                1>"$lout" 2>"$lerr" \
                || fail_trap "$1" "${pids[*]}" "$lout" "$lerr" \
                || return -- "$?"
            for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
            rm -- "$lout" "$lerr"

            ls -- "${2}-default/$dir/"* | while read file; do
                config_copy "$1" 'Snaplet' \
                    "$file" "$2/$dir/$(basename -- "$file")" \
                    || fail_trap "$1" \
                    || return -- "$?"
            done
        done

        task_log "$1" step "Done with $2 particular configs."
    fi
}

# "backend-configs" task
backend_configs_task__covered_by=(all backend)
backend_configs_task() {
    local task_name='backend-configs'
    task_log "$task_name" run

    # srv/snaplets configs
    snaplet_config_copy "$task_name" srv/snaplets

    # carma-mobile-server/snaplets configs
    snaplet_config_copy "$task_name" carma-mobile-server/snaplets

    # Nominatim Mediator config
    config_copy "$task_name" 'Nominatim Mediator' \
        'carma-nominatim-mediator/app.cfg.default' \
        'carma-nominatim-mediator/app.cfg' \
        || fail_trap "$task_name" \
        || return -- "$?"

    # Era Glonass Integration config
    config_copy "$task_name" 'Era Glonass Integration' \
        'carma-era-glonass-integration/app.cfg.default' \
        'carma-era-glonass-integration/app.cfg' \
        || fail_trap "$task_name" \
        || return -- "$?"

    # Config for CaRMa tools
    config_copy "$task_name" 'Tools' \
        'tools/carma-tools.cfg.yaml.example' \
        'tools/carma-tools.cfg.yaml' \
        || fail_trap "$task_name" \
        || return -- "$?"

    task_log "$task_name" done
}

# "backend-carma" task
backend_carma_task__covered_by=(all backend)
backend_carma_task() {
    local task_name='backend-carma'
    task_log "$task_name" run

    if [[ $is_clean_build == true ]]; then
        local clean_flags=()
        if [[ $is_fully_clean_build == true ]]; then
            clean_flags=(--full)
            task_log "$task_name" step 'Cleaning (with --full)…'
        else
            task_log "$task_name" step 'Cleaning…'
        fi

        local app_name="stack clean ${clean_flags[*]}"
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        stack clean "${clean_flags[@]}" \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    local cpus=
    if [[ $is_ci_container == true ]]; then
        # Trying to reduce memory usage on CI
        # (on Circle CI `nproc --all` returns 32).
        cpus=2
    else
        cpus=$(nproc --all)
    fi

    task_log "$task_name" step "Building ($[$cpus] jobs)…"

    local app_name="stack --install-ghc '-j$[$cpus]' build"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    stack --install-ghc "-j$[$cpus]" build \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "backend" task
backend_task__covered_by=(all)
backend_task() {
    local task_name='backend' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_configs_task & pids+=($!)
        backend_carma_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_configs_task
        backend_carma_task
    fi
    task_resolve "$task_name" "$rv"
}

# "backend-test-era-glonass-integration" task
backend_test_era_glonass_integration_task__covered_by=(test backend-test)
backend_test_era_glonass_integration_task() {
    local task_name='backend-test-era-glonass-integration'
    task_log "$task_name" run

    local test_tasks=(
        :carma-era-glonass-integration-types
        :carma-era-glonass-integration-simulate-create-call-card
    )
    local app_name="stack test ${test_tasks[*]}"
    task_log "$task_name" step "$app_name"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    $app_name \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "backend-test-configurator" task
backend_test_configurator_task__covered_by=(test backend-test)
# TODO handle auto build before test
backend_test_configurator_task() {
    local task_name='backend-test-configurator'
    task_log "$task_name" run

    local app_name='Testing example config of "carma-tools"'
    task_log "$task_name" step "$app_name"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd tools && stack exec carma-configurator -- -t carma-tools >/dev/null) \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    # TODO handle running "backend-configs" before this task
    local app_name='Testing real config of "carma-tools"'
    task_log "$task_name" step "$app_name"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd tools && stack exec carma-configurator -- carma-tools >/dev/null) \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "backend-test" task
backend_test_task__covered_by=(test)
# TODO handle auto build before test
backend_test_task() {
    local task_name='backend-test' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_test_configurator_task & pids+=($!)
        backend_test_era_glonass_integration_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_test_configurator_task
        backend_test_era_glonass_integration_task
    fi
    task_resolve "$task_name" "$rv"
}


# "backend-docs-era-glonass-integration" task
# TODO FIXME "haddock" directory could be shared directory between different
#            modules, so cleaning or overwriting could happen.
backend_docs_era_glonass_integration_task__covered_by=(docs backend-docs)
backend_docs_era_glonass_integration_task() {
    local task_name='backend-docs-era-glonass-integration'
    task_log "$task_name" run

    if [[ $is_clean_build == true ]]; then
        task_log "$task_name" step 'Cleaning…'

        local app_name='rm -rf -- haddock'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        $app_name \
            1>"$lout" 2>"$lerr" \
            || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
            || return -- "$?"
        for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
        rm -- "$lout" "$lerr"
    fi

    local pre_cmd='stack exec -- haddock'
    local get_modules_cmd='find carma-era-glonass-integration/src/ -name *.hs'
    local post_cmd='--html --hyperlinked-source -o haddock'
    local app_name=$(
        printf '%s `%s | xargs` %s' "$pre_cmd" "$get_modules_cmd" "$post_cmd"
    )
    local app_cmd=$(
        printf '%s %s %s' "$pre_cmd" "`$get_modules_cmd | xargs`" "$post_cmd"
    )
    task_log "$task_name" step "$app_name"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    $app_cmd \
        1>"$lout" 2>"$lerr" \
        || fail_trap "$task_name" "${pids[*]}" "$lout" "$lerr" \
        || return -- "$?"
    for pid in "${pids[@]}"; do wait -- "$pid"; done && pids=()
    rm -- "$lout" "$lerr"

    task_log "$task_name" done
}

# "backend-docs" task
backend_docs_task__covered_by=(docs)
backend_docs_task() {
    local task_name='backend-docs' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_docs_era_glonass_integration_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_docs_era_glonass_integration_task
    fi
    task_resolve "$task_name" "$rv"
}


# "all" task
all_task__covered_by=()
all_task() {
    local task_name='all' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_task & pids+=($!)
        frontend_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_task
        frontend_task
    fi
    task_resolve "$task_name" "$rv"
}

# "test" task
test_task__covered_by=()
test_task() {
    local task_name='test' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_test_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_test_task
    fi
    task_resolve "$task_name" "$rv"
}

# "docs" task
docs_task__covered_by=()
docs_task() {
    local task_name='docs' rv=0
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_docs_task & pids+=($!)
        for pid in "${pids[@]}"; do wait -- "$pid" || rv=$?; done
    else
        backend_docs_task
    fi
    task_resolve "$task_name" "$rv"
}


# *** END OF TASKS SECTION ***


# A helper that checks if a task is already included into another specified
# complex (multiple) task. In that case it fails whole script to avoid
# unexpected behavior.
#
# Usage example:
#   solo "$task" "${some_cmd_task__covered_by[@]}"
#
solo() {
    task_to_log=$1
    shift
    for task in "${positional[@]}"; do
        if elem "$task" "$@"; then
            printf \
                '"%s" is already covered by "%s"!\n' \
                "$task_to_log" "$task" >&2
            exit 1
        fi
    done
}

# Detecting already covered tasks error first
for task in "${positional[@]}"; do
    case $task in
        all)
            solo "$task" "${all_task__covered_by[@]}"
            ;;
        test)
            solo "$task" "${test_task__covered_by[@]}"
            ;;
        docs)
            solo "$task" "${docs_task__covered_by[@]}"
            ;;
        backend)
            solo "$task" "${backend_task__covered_by[@]}"
            ;;
        backend-configs)
            solo "$task" "${backend_configs_task__covered_by[@]}"
            ;;
        backend-carma)
            solo "$task" "${backend_carma_task__covered_by[@]}"
            ;;
        backend-test)
            solo "$task" "${backend_test_task__covered_by[@]}"
            ;;
        backend-test-configurator)
            solo "$task" "${backend_test_configurator_task__covered_by[@]}"
            ;;
        backend-test-era-glonass-integration)
            solo "$task" \
                "${backend_test_era_glonass_integration_task__covered_by[@]}"
            ;;
        backend-docs)
            solo "$task" "${backend_docs_task__covered_by[@]}"
            ;;
        backend-docs-era-glonass-integration)
            solo "$task" \
                "${backend_docs_era_glonass_integration_task__covered_by[@]}"
            ;;
        frontend)
            solo "$task" "${frontend_task__covered_by[@]}"
            ;;
        frontend-pure)
            solo "$task" "${frontend_pure_task__covered_by[@]}"
            ;;
        frontend-legacy)
            solo "$task" "${frontend_legacy_task__covered_by[@]}"
            ;;
        frontend-backend-templates)
            solo "$task" "${frontend_backend_templates_task__covered_by[@]}"
            ;;
        *)
            printf \
                'Unknown "%s" task, probably error in the script!\n' \
                "$task" >&2
            exit 1
            ;;
    esac
done


# Running tasks
[[ $run_in_parallel == true ]] && task_pids=()
for task in "${positional[@]}"; do
    case $task in
        all)
            if [[ $run_in_parallel == true ]]
                then all_task & task_pids+=($!)
                else all_task; fi
            ;;
        test)
            if [[ $run_in_parallel == true ]]
                then test_task & task_pids+=($!)
                else test_task; fi
            ;;
        docs)
            if [[ $run_in_parallel == true ]]
                then docs_task & task_pids+=($!)
                else docs_task; fi
            ;;
        backend)
            if [[ $run_in_parallel == true ]]
                then backend_task & task_pids+=($!)
                else backend_task; fi
            ;;
        backend-configs)
            if [[ $run_in_parallel == true ]]
                then backend_configs_task & task_pids+=($!)
                else backend_configs_task; fi
            ;;
        backend-carma)
            if [[ $run_in_parallel == true ]]
                then backend_carma_task & task_pids+=($!)
                else backend_carma_task; fi
            ;;
        backend-test)
            if [[ $run_in_parallel == true ]]
                then backend_test_task & task_pids+=($!)
                else backend_test_task; fi
            ;;
        backend-test-configurator)
            if [[ $run_in_parallel == true ]]
                then backend_test_configurator_task & task_pids+=($!)
                else backend_test_configurator_task; fi
            ;;
        backend-test-era-glonass-integration)
            if [[ $run_in_parallel == true ]]
                then backend_test_era_glonass_integration_task & task_pids+=($!)
                else backend_test_era_glonass_integration_task; fi
            ;;
        backend-docs)
            if [[ $run_in_parallel == true ]]
                then backend_docs_task & task_pids+=($!)
                else backend_docs_task; fi
            ;;
        backend-docs-era-glonass-integration)
            if [[ $run_in_parallel == true ]]
                then backend_docs_era_glonass_integration_task & task_pids+=($!)
                else backend_docs_era_glonass_integration_task; fi
            ;;
        frontend)
            if [[ $run_in_parallel == true ]]
                then frontend_task & task_pids+=($!)
                else frontend_task; fi
            ;;
        frontend-pure)
            if [[ $run_in_parallel == true ]]
                then frontend_pure_task & task_pids+=($!)
                else frontend_pure_task; fi
            ;;
        frontend-legacy)
            if [[ $run_in_parallel == true ]]
                then frontend_legacy_task & task_pids+=($!)
                else frontend_legacy_task; fi
            ;;
        frontend-backend-templates)
            if [[ $run_in_parallel == true ]]
                then frontend_backend_templates_task & task_pids+=($!)
                else frontend_backend_templates_task; fi
            ;;
        *)
            printf \
                'Unknown "%s" task, probably error in the script!\n' \
                "$task" >&2
            exit 1
            ;;
    esac
done

if [[ $run_in_parallel == true ]]; then
    rv=0
    for pid in "${task_pids[@]}"; do wait -- "$pid" || rv=$?; done
    if (( $rv != 0 )); then exit -- "$rv"; fi
fi
