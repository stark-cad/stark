# Sets a valid graphics API for builds
# TODO: use a git pre-commit hook to enforce empty backend?

set -e

if [ -z $1 ]
then
    # Outputs the lines that set graphics API for Cargo builds
    echo "current api setting:"
    sed -n '/^pa.*gfx..*backend[-/].*"$/p' Cargo.toml
else
    if [[ $1 == "empty" ]] || [[ $1 == "vulkan" ]] || [[ $1 == "dx12" ]] || [[ $1 == "metal" ]]
    then
        strng="s/\(^pa.*gfx..*backend[-/]\).*\"$/\1${1}\"/"
        sed -i $strng Cargo.toml
        echo "gfx-hal backend api set to ${1}"
    else
        echo "invalid backend"
    fi
fi
