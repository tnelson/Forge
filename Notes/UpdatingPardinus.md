# To update Pardinus

## Building native libraries (if needed)

These include Glucose, lingeling, minisat, and minisatprover.

The build scripts live in the Pardinus repo, under jni/ (see `jni/BUILDING_NATIVE.md` there).

From the Pardinus repo, run 
  * `build-linux.sh` (once per arch) 
  * `build-macos.sh`
  * `build-windows.sh`.
  
Install each platform's output into its <forge-repo>/forge/pardinus-cli/jar/native/<dir>:
  * `linux-x86_64`
  * `linux-aarch64` 
  * `macos`
  * `windows-x86_64`
  * `windows-arm64`
  
Then run the test script in `<forge-repo>/forge/pardinus-cli/jar/native`: `./test-native.sh --dir <dir>` (This script is run by the `nativeLibraries` CI workflow.)

The `MANIFEST.txt` files for each native library give the Pardinus commit hash each was built from. 

## Building and installing the JAR

From command line:
1. `cd` into whichever directory the `Pardinus` repo lives.
2. git checkout <branch desired; likely master>
3. `cd <forge-repo-parent-dir>/Forge/forge/pardinus-cli`
4. `racket install-pardinus-jar.rkt <pardinus-repo-parent-dir>/Pardinus`
5. `raco setup forge` 

The installed JAR's sha256 will differ, even if the builds are identical, because Maven 
writes timestamps into each JAR. 
