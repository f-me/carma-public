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


# $1 is task name.
# [[ $2 == run ]] means task is running
# [[ $2 == done ]] means task is done
# [[ $2 == step ]] to log particular sub-tasks
# $3 is log message when [[ $2 == step ]]
task_log() {
    local d=$(date '+%Y-%m-%d %H:%M:%S')
    if [[ $2 == run ]]; then
        printf '[%s] "%s" task is running...\n' "$d" "$1"
    elif [[ $2 == done ]]; then
        printf '[%s] "%s" task is done.\n' "$d" "$1"
    elif [[ $2 == step ]]; then
        printf '[%s] "%s" task: %s\n' "$d" "$1" "$3"
    else
        printf '[%s] Unexpected "%s" task "%s" action!\n' "$d" "$1" "$2" >&2
        exit 1
    fi
}


frontend_pure_task__covered_by=(frontend all)
frontend_pure_task() {
    task_log 'frontend-pure' run
    local dir='srv/resources/assets/pure'

    # Installing dependencies
    (cd -- "$dir" && npm install)

    # Fetching submodules for Circle CI container
    if [[ $is_ci_container == true ]]; then
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-dropzone)
        (cd -- "$dir" && \
            git submodule update --init --recursive \
            purescript-react-rich-text-editor)
    fi

    local ci_flags=()
    # For Circle CI container allowing installing packages by superuser
    [[ $is_ci_container == true ]] && ci_flags=('--allow-root')

    # Installing PureScript dependencies
    (cd -- "$dir" && npm run bower -- install "${ci_flags[@]}")

    local clean_infix=
    [[ $is_clean_build == true ]] && clean_infix='clean-'

    local prod_prefix=
    [[ $is_production_build == true ]] && prod_prefix='prod-'

    (cd -- "$dir" && npm run "${prod_prefix}${clean_infix}build")

    task_log 'frontend-pure' done
}

# Preparation for legacy frontend build.
# Helps to make it for both "frontend-legacy" and "frontend-backend-templates"
# tasks when they're both specified, no need to do it twice.
frontend_legacy_pre() {
    local dir='srv'
    (cd -- "$dir" && npm install)
}

frontend_legacy_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
frontend_legacy_task() {
    task_log 'frontend-legacy' run
    local dir='srv'
    [[ $1 == true ]] || frontend_legacy_pre

    local clean_infix=
    [[ $is_clean_build == true ]] && clean_infix='clean-'

    local prod_prefix=
    [[ $is_production_build == true ]] && prod_prefix='prod-'

    (cd -- "$dir" && npm run "${prod_prefix}${clean_infix}build")

    task_log 'frontend-legacy' done
}

frontend_backend_templates_task__covered_by=(frontend all)
# [[ $1 == true ]] means that preparations is already done.
# It's part of a legacy frontend, so it's kinda connected to it
# (that's why they're both shared "frontend_legacy_pre").
frontend_backend_templates_task() {
    task_log 'frontend-backend-templates' run
    local dir='srv'
    [[ $1 == true ]] || frontend_legacy_pre

    local clean_prefix=
    [[ $is_clean_build == true ]] && clean_prefix='clean-'

    (cd -- "$dir" && npm run "${clean_prefix}build-backend-templates")

    task_log 'frontend-backend-templates' done
}

# Helper to run legacy tasks in parallel
frontend_task_parallel_legacy() {
    frontend_legacy_task true &
    frontend_backend_templates_task true &
    wait
}

frontend_task__covered_by=(all)
frontend_task() {
    task_log 'frontend' run
    if [[ $run_in_parallel == true ]]; then
        frontend_pure_task &
        (frontend_legacy_pre && frontend_task_parallel_legacy) &
        wait
    else
        frontend_pure_task
        frontend_legacy_pre
        frontend_legacy_task true
        frontend_backend_templates_task true
    fi
    task_log 'frontend' done
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
        cp -- "$3" "$4"
        task_log "$1" step $"$2 config file \"$4\" is copied."
    fi
}

backend_configs_task__covered_by=(all backend)
backend_configs_task() {
    task_log 'backend-configs' run

    # Snaplets configs
    task_log 'backend-configs' step 'Checking snaplets configs directory...'
    if [[ ! -d srv/snaplets ]]; then
        local x=$'Snaplets configs directory doesn\'t exists, copying it'
        x="$x from srv/snaplets-default to srv/snaplets..."
        task_log 'backend-configs' step "$x"
        cp -r srv/snaplets-default srv/snaplets
        task_log 'backend-configs' step 'Snaplets configs directory is copied.'
    else
        task_log 'backend-configs' step \
            'Snaplets configs directory exists, checking particular configs...'

        ls srv/snaplets-default | while read dir; do
            mkdir -p -- "srv/snaplets/$dir"
            ls -- "srv/snaplets-default/$dir/"* | while read file; do
                config_copy 'backend-configs' 'Snaplet' \
                    "$file" "srv/snaplets/$dir/$(basename -- "$file")"
            done
        done

        task_log 'backend-configs' step 'Done with snaplets particular configs.'
    fi

    # Nominatim Mediator config
    config_copy 'backend-configs' 'Nominatim Mediator' \
        'carma-nominatim-mediator/app.cfg.default' \
        'carma-nominatim-mediator/app.cfg'

    # Config for CaRMa tools
    config_copy 'backend-configs' 'Tools' \
        'tools/carma-tools.cfg.yaml.example' \
        'tools/carma-tools.cfg.yaml'

    task_log 'backend-configs' done
}

backend_carma_task__covered_by=(all backend)
backend_carma_task() {
    task_log 'backend-carma' run

    if [[ $is_clean_build == true ]]; then
        task_log 'backend-carma' step 'Cleaning...'
        local clean_flags=(--full)
        [[ $is_clean_soft == true ]] && clean_flags=()
        stack clean "${clean_flags[@]}"
    fi

    local cpus=
    if [[ $is_ci_container == true ]]; then
        # Trying to reduce memory usage on CI
        # (on Circle CI `nproc --all` returns 32).
        cpus=2
    else
        cpus=$(nproc --all)
    fi

    task_log 'backend-carma' step "Building ($[$cpus] jobs)..."
    stack --install-ghc "-j$[$cpus]" build

    task_log 'backend-carma' done
}

backend_task__covered_by=(all)
backend_task() {
    task_log 'backend' run
    if [[ $run_in_parallel == true ]]; then
        backend_configs_task &
        backend_carma_task &
        wait
    else
        backend_configs_task
        backend_carma_task
    fi
    task_log 'backend' done
}

backend_test_task__covered_by=(test)
# TODO handle auto build before test
backend_test_task() {
    task_log 'backend-test' run
    (cd tools && stack exec carma-configurator -- -t carma-tools >/dev/null)
    task_log 'backend-test' done
}


all_task__covered_by=()
all_task() {
    task_log 'all' run
    if [[ $run_in_parallel == true ]]; then
        backend_task &
        frontend_task &
        wait
    else
        backend_task
        frontend_task
    fi
    task_log 'all' done
}

test_task__covered_by=()
# TODO handle auto build before test
test_task() {
    task_log 'test' run
    if [[ $run_in_parallel == true ]]; then
        backend_test_task &
        wait
    else
        backend_test_task
    fi
    task_log 'test' done
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
