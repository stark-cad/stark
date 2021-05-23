#TODO: use a git pre-commit hook to enforce empty backend?

set -e

if [[ $1 == "empty" ]] || [[ $1 == "vulkan" ]] || [[ $1 == "dx12" ]] || [[ $1 == "metal" ]]
then
    strng="s/\(^pa.*gfx..*backend[-/]\).*\"$/\1${1}\"/"
    sed -i $strng Cargo.toml
    echo "gfx-hal backend set to ${1}"
else
    echo "invalid backend"
fi
