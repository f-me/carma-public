#!/bin/bash -e

# Abstract helper for building CaRMa.

# Use -h or --help option to show it
show_usage() {
    cat << USAGE
Usage: $0 [-c|--clean] [--soft-clean] [-p|--parallel] [--production] [--ci] COMMANDS...

Commands (multiple tasks):
    $0 all       Build everything
    $0 backend   Build backend
    $0 frontend  Build frontend
    $0 test      Run all tests

Commands (single task):
    $0 backend-configs             Copy configs examples if needed
    $0 backend-carma               Just build CaRMa backend
    $0 backend-test                Run all tests for backend
    $0 frontend-pure               Build "pure" frontend
    $0 frontend-legacy             Build "legacy" frontend
    $0 frontend-backend-templates  Build templates which used by backend

Options:
    -c, --clean     Clean previous bundles before build
    --soft-clean    Softly clean if possible
                    (for backend means don't fully clean .stack-work but just
                    built CaRMa modules, useful for reducing build time)
    -p, --parallel  If a command have multiple tasks they run in parallel
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
    value=$1
    shift
    for it in "$@"; do
        [[ $it == $value ]] && return 0
    done
    return 1
}

# Working directory is CaRMa's root
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

positional=() # COMMANDS
positional_split=false
run_in_parallel=false
is_production_build=false
is_ci_container=false
is_clean_build=false
is_clean_soft=false

available_commands=(
    all test
    backend backend-configs backend-carma backend-test
    frontend frontend-pure frontend-legacy frontend-backend-templates
)

for arg in "$@"; do
    if [[ $positional_split == true ]]; then
        if elem "$arg" "${available_commands[@]}"; then
            positional+=("$arg")
        else
            printf 'Unknown command: "%s"\n' "$arg" >&2
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
            --soft-clean)
                is_clean_build=true
                is_clean_soft=true
                ;;
            -p|--parallel)
                run_in_parallel=true
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
                if elem "$arg" "${available_commands[@]}"; then
                    positional+=("$arg")
                else
                    printf 'Unknown command: "%s"\n' "$arg" >&2
                    show_usage >&2
                    exit 1
                fi
        esac
    fi
done

if (( ${#positional[*]} == 0 )); then
    echo 'No command specified!' >&2
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


# $1 is task name.
# [[ $2 == run ]] means task is running
# [[ $2 == done ]] means task is done
# [[ $2 == step ]] to log particular sub-tasks
# [[ $2 == app-stdout ]] to log application's messsage
# [[ $2 == app-stderr ]] to log application's error message
# $3 is log message when [[ $2 == step ]]
# $4 is application name when [[ $2 == app-stdout ]] || [[ $2 == app-stderr ]]
task_log() {
    (
    local d=$(date '+%Y-%m-%d %H:%M:%S')

    if [[ $2 == run ]]; then
        printf '[%s] "%s" task is running...\n' "$d" "$1"
    elif [[ $2 == done ]]; then
        printf '[%s] "%s" task is done.\n' "$d" "$1"
    elif [[ $2 == step ]]; then
        printf '[%s] "%s" task: %s\n' "$d" "$1" "$3"

    elif [[ $2 == app-stdout ]]; then
        printf '[%s] "%s" task "%s" app [STDOUT]: %s\n' "$d" "$1" "$4" "$3"
    elif [[ $2 == app-stderr ]]; then
        printf '[%s] "%s" task "%s" app [STDERR]: %s\n' "$d" "$1" "$4" "$3" >&2

    else
        printf '[%s] Unexpected "%s" task "%s" action!\n' "$d" "$1" "$2" >&2
        exit 1
    fi
    ) 1>&3 2>&4
}

# $1 - '1' for stdout and '2' for stderr
# $2 - FIFO file descriptor
# $3 - task name to log
# $4 - application name to log
# Usage example:
#   local out_fifo=$(mk_tmp_fifo 1 'task name' 'app name')
#   local err_fifo=$(mk_tmp_fifo 2 'task name' 'app name')
#   ...
#   kill_loggers "$out_fifo" "$err_fifo"
app_logger() {
    local action=
    (( $1 == 1 )) && action=app-stdout || action=app-stderr

    cat -- "$2" | while IFS= read -r x; do
        task_log "$3" "$action" "$x" "$4"
    done
}


frontend_pure_task__covered_by=(frontend all)
frontend_pure_task() {
    local task_name='frontend-pure' dir='srv/resources/assets/pure'
    task_log "$task_name" run

    # Installing dependencies
    task_log "$task_name" step 'Installing dependencies...'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" 'npm install' & pids+=($!)
    app_logger 2 "$lerr" "$task_name" 'npm install' & pids+=($!)
    (cd -- "$dir" && npm install) 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    # Fetching submodules for Circle CI container
    if [[ $is_ci_container == true ]]; then
        task_log "$task_name" step 'Cloning git submodules...'

        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        local app_name='purescript-react-dropzone git submodule'
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-dropzone) 1>"$lout" 2>"$lerr"
        wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        local app_name='purescript-react-rich-text-editor git submodule'
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-rich-text-editor) 1>"$lout" 2>"$lerr"
        wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()
    fi

    local ci_flags=()
    # For Circle CI container allowing installing packages by superuser
    [[ $is_ci_container == true ]] && ci_flags=('--allow-root')

    # Installing PureScript dependencies
    task_log "$task_name" step \
        'Installing PureScript dependencies (using bower)...'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" 'bower install' & pids+=($!)
    app_logger 2 "$lerr" "$task_name" 'bower install' & pids+=($!)
    (cd -- "$dir" && npm run bower -- install "${ci_flags[@]}") \
        1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    local clean_infix=
    [[ $is_clean_build == true ]] && clean_infix='clean-'

    local prod_prefix=
    [[ $is_production_build == true ]] && prod_prefix='prod-'

    local npm_task=${prod_prefix}${clean_infix}build
    task_log "$task_name" step $"Running npm \"$npm_task\" script..."
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "npm run $npm_task" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "npm run $npm_task" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    task_log 'frontend-pure' done
}

# Preparation for legacy frontend build.
# Helps to make it for both "frontend-legacy" and "frontend-backend-templates"
# tasks when they're both specified, no need to do it twice.
frontend_legacy_pre() {
    local task_name='frontend-legacy[pre]' dir='srv'

    task_log "$task_name" step 'Installing dependencies...'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" 'npm install' & pids+=($!)
    app_logger 2 "$lerr" "$task_name" 'npm install' & pids+=($!)
    (cd -- "$dir" && npm install) 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()
}

frontend_legacy_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
frontend_legacy_task() {
    local task_name='frontend-legacy' dir='srv'
    task_log "$task_name" run
    [[ $1 == true ]] || frontend_legacy_pre

    local clean_infix=
    [[ $is_clean_build == true ]] && clean_infix='clean-'

    local prod_prefix=
    [[ $is_production_build == true ]] && prod_prefix='prod-'

    local npm_task="${prod_prefix}${clean_infix}build"
    task_log "$task_name" step $"Running npm \"$npm_task\" script..."
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "npm run $npm_task" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "npm run $npm_task" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    task_log "$task_name" done
}

frontend_backend_templates_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
# It's part of a legacy frontend, so it's kinda connected to it
# (that's why they're both shared "frontend_legacy_pre").
frontend_backend_templates_task() {
    local task_name='frontend-backend-templates' dir='srv'
    task_log "$task_name" run
    [[ $1 == true ]] || frontend_legacy_pre

    local clean_prefix=
    [[ $is_clean_build == true ]] && clean_prefix='clean-'

    local npm_task=${clean_prefix}build-backend-templates
    task_log "$task_name" step $"Running npm \"$npm_task\" script..."
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "npm run $npm_task" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "npm run $npm_task" & pids+=($!)
    (cd -- "$dir" && npm run "$npm_task") 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    task_log "$task_name" done
}

# Helper to run legacy tasks in parallel
frontend_task_parallel_legacy() {
    local pids=()
    frontend_legacy_task true & pids+=($!)
    frontend_backend_templates_task true & pids+=($!)
    wait -- "${pids[@]}"
}

frontend_task__covered_by=(all)
frontend_task() {
    local task_name='frontend'

    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        frontend_pure_task & pids+=($!)
        (frontend_legacy_pre && frontend_task_parallel_legacy) & pids+=($!)
        wait -- "${pids[@]}"
    else
        frontend_pure_task
        frontend_legacy_pre
        frontend_legacy_task true
        frontend_backend_templates_task true
    fi
    task_log "$task_name" done
}


# $1 - task name for log
# $2 - label of a config
# $3 - config example file (as a source to copy from)
# $4 - destination of a config
config_copy() {
    task_log "$1" step $"Checking $2 config file: \"$4\"..."
    if [[ -f $4 ]]; then
        task_log "$1" step $"$2 config file \"$4\" exists."
    else
        local x=$"$2 config file \"$4\" doesn't exist,"
        x=$"$x copying from \"$3\" to \"$4\"..."
        task_log "$1" step "$x"

        local app_name="cp -- '$3' '$4'"
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$1" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$1" "$app_name" & pids+=($!)
        cp -- "$3" "$4" 1>"$lout" 2>"$lerr"
        wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

        task_log "$1" step $"$2 config file \"$4\" is copied."
    fi
}

backend_configs_task__covered_by=(all backend)
backend_configs_task() {
    local task_name='backend-configs'
    task_log "$task_name" run

    # Snaplets configs
    task_log "$task_name" step 'Checking snaplets configs directory...'
    if [[ ! -d srv/snaplets ]]; then
        local x=$'Snaplets configs directory doesn\'t exists, copying it'
        x="$x from srv/snaplets-default to srv/snaplets..."
        task_log "$task_name" step "$x"

        local app_name='cp -r srv/snaplets-default srv/snaplets'
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        cp -r srv/snaplets-default srv/snaplets 1>"$lout" 2>"$lerr"
        wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

        task_log "$task_name" step 'Snaplets configs directory is copied.'
    else
        task_log "$task_name" step \
            'Snaplets configs directory exists, checking particular configs...'

        ls srv/snaplets-default | while read dir; do
            local app_name="mkdir -p -- 'srv/snaplets/$dir'"
            local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
            app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
            app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
            mkdir -p -- "srv/snaplets/$dir" 1>"$lout" 2>"$lerr"
            wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

            ls -- "srv/snaplets-default/$dir/"* | while read file; do
                config_copy "$task_name" 'Snaplet' \
                    "$file" "srv/snaplets/$dir/$(basename -- "$file")"
            done
        done

        task_log "$task_name" step 'Done with snaplets particular configs.'
    fi

    # Nominatim Mediator config
    config_copy "$task_name" 'Nominatim Mediator' \
        'carma-nominatim-mediator/app.cfg.default' \
        'carma-nominatim-mediator/app.cfg'

    # Config for CaRMa tools
    config_copy "$task_name" 'Tools' \
        'tools/carma-tools.cfg.yaml.example' \
        'tools/carma-tools.cfg.yaml'

    task_log "$task_name" done
}

backend_carma_task__covered_by=(all backend)
backend_carma_task() {
    local task_name='backend-carma'
    task_log "$task_name" run

    if [[ $is_clean_build == true ]]; then
        task_log "$task_name" step 'Cleaning...'
        local clean_flags=(--full)
        [[ $is_clean_soft == true ]] && clean_flags=()

        local app_name="stack clean ${clean_flags[*]}"
        local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
        app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
        app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
        stack clean "${clean_flags[@]}" 1>"$lout" 2>"$lerr"
        wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()
    fi

    local cpus=
    if [[ $is_ci_container == true ]]; then
        # Trying to reduce memory usage on CI
        # (on Circle CI `nproc --all` returns 32).
        cpus=2
    else
        cpus=$(nproc --all)
    fi

    task_log "$task_name" step "Building ($[$cpus] jobs)..."

    local app_name="stack --install-ghc '-j$[$cpus]' build"
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    stack --install-ghc "-j$[$cpus]" build 1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    task_log "$task_name" done
}

backend_task__covered_by=(all)
backend_task() {
    local task_name='backend'
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_configs_task & pids+=($!)
        backend_carma_task & pids+=($!)
        wait -- "${pids[@]}"
    else
        backend_configs_task
        backend_carma_task
    fi
    task_log "$task_name" done
}

backend_test_task__covered_by=(test)
# TODO handle auto build before test
backend_test_task() {
    local task_name='backend-test'
    task_log "$task_name" run

    local app_name='carma-tools example config testing'
    local lout=$(mk_tmp_fifo) lerr=$(mk_tmp_fifo) pids=()
    app_logger 1 "$lout" "$task_name" "$app_name" & pids+=($!)
    app_logger 2 "$lerr" "$task_name" "$app_name" & pids+=($!)
    (cd tools && stack exec carma-configurator -- -t carma-tools >/dev/null) \
        1>"$lout" 2>"$lerr"
    wait -- "${pids[@]}" && rm -- "$lout" "$lerr" && pids=()

    task_log "$task_name" done
}


all_task__covered_by=()
all_task() {
    local task_name='all'
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_task & pids+=($!)
        frontend_task & pids+=($!)
        wait -- "${pids[@]}"
    else
        backend_task
        frontend_task
    fi
    task_log "$task_name" done
}

test_task__covered_by=()
# TODO handle auto build before test
test_task() {
    local task_name='test'
    task_log "$task_name" run
    if [[ $run_in_parallel == true ]]; then
        local pids=()
        backend_test_task & pids+=($!)
        wait -- "${pids[@]}"
    else
        backend_test_task
    fi
    task_log "$task_name" done
}


# A helper that checks if a command is already included to another specified
# complex command. In that case it fails whole script to avoid unexpected
# behavior.
#
# Usage example:
#   already_covered_handle "$cmd" "${some_cmd_task__covered_by[@]}"
#
already_covered_handle() {
    cmd_to_log=$1
    shift
    for cmd in "${positional[@]}"; do
        if elem "$cmd" "$@"; then
            printf '"%s" is already covered by "%s"!\n' "$cmd_to_log" "$cmd" >&2
            exit 1
        fi
    done
}


for cmd in "${positional[@]}"; do
    case $cmd in
        all)
            already_covered_handle "$cmd" "${all_task__covered_by[@]}"
            all_task
            ;;
        test)
            already_covered_handle "$cmd" "${test_task__covered_by[@]}"
            test_task
            ;;
        backend)
            already_covered_handle "$cmd" "${backend_task__covered_by[@]}"
            backend_task
            ;;
        backend-configs)
            already_covered_handle "$cmd" \
                "${backend_configs_task__covered_by[@]}"
            backend_configs_task
            ;;
        backend-carma)
            already_covered_handle "$cmd" \
                "${backend_carma_task__covered_by[@]}"
            backend_carma_task
            ;;
        backend-test)
            already_covered_handle "$cmd" "${backend_test_task__covered_by[@]}"
            backend_test_task
            ;;
        frontend)
            already_covered_handle "$cmd" "${frontend_task__covered_by[@]}"
            frontend_task
            ;;
        frontend-pure)
            already_covered_handle "$cmd" "${frontend_pure_task__covered_by[@]}"
            frontend_pure_task
            ;;
        frontend-legacy)
            already_covered_handle "$cmd" \
                "${frontend_legacy_task__covered_by[@]}"
            frontend_legacy_task
            ;;
        frontend-backend-templates)
            already_covered_handle "$cmd" \
                "${frontend_backend_templates_task__covered_by[@]}"
            frontend_backend_templates_task
            ;;
        *)
            printf \
                'Unknown "%s" command, probably error in the script!\n' \
                "$cmd" >&2
            exit 1
            ;;
    esac
done

# cleanup
exec 3>&- 4>&-
wait -- "${logger_pids[@]}"
rm -- "$stdout_fifo" "$stderr_fifo"
