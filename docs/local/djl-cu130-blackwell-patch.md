# DJL CUDA 13.0 / Blackwell (sm_120) Support

## Summary
This document describes the changes needed to add CUDA 13.0 (cu130) and NVIDIA Blackwell architecture (sm_120, e.g. RTX 5090) support to DJL.

## Testing Environment
- GPU: NVIDIA GeForce RTX 5090 (Blackwell architecture, sm_120)
- CUDA Toolkit: 13.1
- PyTorch: 2.9.1+cu130
- DJL: 0.36.0
- Platform: Ubuntu 24.04 on WSL2

## Changes Required

### 1. pytorch-native/build.sh
The build script needs to be updated to:
- Add `cu130` to the list of supported CUDA flavors
- Handle the different libtorch naming scheme for cu130 (no cxx11-abi prefix)

```diff
-    if [[ ! "$FLAVOR" =~ ^(cpu|cu117|cu121|cu124|cu128)$ ]]; then
+    if [[ ! "$FLAVOR" =~ ^(cpu|cu117|cu121|cu124|cu128|cu130)$ ]]; then

     else
-      curl -s "https://download.pytorch.org/libtorch/${FLAVOR}/libtorch${CXX11ABI}-shared-with-deps-${VERSION}%2B${FLAVOR}.zip" | jar xv >/dev/null
+      if [[ "$FLAVOR" == "cu130" ]]; then
+        # cu130 uses different naming (no cxx11-abi prefix)
+        curl -s "https://download.pytorch.org/libtorch/${FLAVOR}/libtorch-shared-with-deps-${VERSION}%2B${FLAVOR}.zip" | jar xv >/dev/null
+      else
+        curl -s "https://download.pytorch.org/libtorch/${FLAVOR}/libtorch${CXX11ABI}-shared-with-deps-${VERSION}%2B${FLAVOR}.zip" | jar xv >/dev/null
+      fi
```

### 2. libtorch 2.9.1+cu130 Cleanup
The official libtorch cu130 distribution includes Python-dependent libraries that break JNI loading:
- `libtorch_python.so` - Requires Python symbols (PyInstanceMethod_Type)
- `libnnapi_backend.so` - Also depends on Python

These should be removed or excluded from loading.

### 3. libcudart.so Symlink
The bundled cudart has a hash in the filename (e.g., `libcudart-6876f484.so.13`).
A symlink `libcudart.so` pointing to the actual file is needed for JNA to find it.

### 4. JNA Library Path
The JNA library path must be set to the libtorch lib directory so that JNA loads the bundled cudart
instead of potentially incompatible system CUDA libraries:
```
-Djna.library.path=/path/to/libtorch/lib
```

## Build Instructions

```bash
cd ~/packages/deepjavalibrary/djl/engines/pytorch/pytorch-native

# Build JNI for cu130 with PyTorch 2.9.1
./build.sh 2.9.1 cu130

# Copy the built library to DJL cache
mkdir -p ~/.djl.ai/pytorch/2.9.1-cu130-linux-x86_64/
cp build/libdjl_torch.so ~/.djl.ai/pytorch/2.9.1-cu130-linux-x86_64/0.36.0-libdjl_torch.so
```

## Usage

Use the setup script (sets `SBT_OPTS` automatically):
```bash
source scripts/setup-pytorch-cu130.sh
sbt 'rlLogicJVM/Test/runMain cps.learning.examples.tiktaktoe.SelfPlayTrainer'
```

Or set environment variables manually:
```bash
export LIBTORCH_PATH=/path/to/libtorch-2.9.1-cu130/lib
export LD_LIBRARY_PATH=$LIBTORCH_PATH:$LD_LIBRARY_PATH
export PYTORCH_LIBRARY_PATH=$LIBTORCH_PATH
export PYTORCH_VERSION=2.9.1
export PYTORCH_FLAVOR=cu130

sbt -J-Djna.library.path=$LIBTORCH_PATH 'rlLogicJVM/Test/runMain cps.learning.examples.tiktaktoe.SelfPlayTrainer'
```

## Verification
Successful GPU detection output:
```
GPU detected: 1 GPU(s) available, using GPU 0
```

## Known Issues

1. **PTX JIT Compiler Crash**: If the wrong libcudart is loaded (e.g., system CUDA 13.1 instead of bundled cu130),
   the PTX JIT compiler may crash with SIGSEGV in `libnvidia-ptxjitcompiler.so.1`.
   This is resolved by ensuring the bundled cudart is loaded via `-Djna.library.path`.

2. **Predictor not closed warnings**: These are non-critical warnings about resource cleanup.
