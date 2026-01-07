#!/bin/bash
# Setup script for DJL PyTorch 2.9.1 with CUDA 13.0 (cu130)
# Supports Blackwell (sm_120), Ada Lovelace (sm_89), and other CUDA 13.0 compatible GPUs

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
LIBTORCH_PATH="$PROJECT_DIR/lib/libtorch-2.9.1-cu130/lib"

# Check if libtorch exists
if [ ! -d "$LIBTORCH_PATH" ]; then
    echo "Error: libtorch not found at $LIBTORCH_PATH"
    echo "Please download libtorch cu130 from:"
    echo "  https://download.pytorch.org/libtorch/cu130/libtorch-shared-with-deps-2.9.1%2Bcu130.zip"
    exit 1
fi

# Create libcudart.so symlink if missing
if [ ! -e "$LIBTORCH_PATH/libcudart.so" ]; then
    CUDART_FILE=$(ls "$LIBTORCH_PATH"/libcudart-*.so.* 2>/dev/null | head -1)
    if [ -n "$CUDART_FILE" ]; then
        ln -sf "$(basename "$CUDART_FILE")" "$LIBTORCH_PATH/libcudart.so"
        echo "Created symlink: libcudart.so -> $(basename "$CUDART_FILE")"
    fi
fi

# Remove Python-dependent libraries that break JNI loading
if [ -f "$LIBTORCH_PATH/libtorch_python.so" ]; then
    rm -f "$LIBTORCH_PATH/libtorch_python.so"
    echo "Removed libtorch_python.so (Python dependency)"
fi

if [ -f "$LIBTORCH_PATH/libnnapi_backend.so" ]; then
    rm -f "$LIBTORCH_PATH/libnnapi_backend.so"
    echo "Removed libnnapi_backend.so (Python dependency)"
fi

# Export environment variables
export PATH=/usr/local/cuda/bin:$PATH
export LD_LIBRARY_PATH=$LIBTORCH_PATH:$LD_LIBRARY_PATH
export PYTORCH_LIBRARY_PATH=$LIBTORCH_PATH
export PYTORCH_VERSION=2.9.1
export PYTORCH_FLAVOR=cu130

# Set JNA library path for sbt (so it finds the bundled cudart)
export SBT_OPTS="${SBT_OPTS} -Djna.library.path=$LIBTORCH_PATH"

echo ""
echo "GPU environment configured:"
echo "  PYTORCH_LIBRARY_PATH=$PYTORCH_LIBRARY_PATH"
echo "  PYTORCH_VERSION=$PYTORCH_VERSION"
echo "  PYTORCH_FLAVOR=$PYTORCH_FLAVOR"
echo "  SBT_OPTS includes -Djna.library.path"
echo ""
echo "To run with GPU:"
echo "  source $SCRIPT_DIR/setup-pytorch-cu130.sh"
echo "  sbt 'rlLogicJVM/Test/runMain cps.learning.examples.tiktaktoe.SelfPlayTrainer'"
