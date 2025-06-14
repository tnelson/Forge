This folder contains a build of the current version of Sterling for Forge. To update, build Sterling and recursively copy the contents of the `dist` folder in that repo to the `build` sub-folder here. Remember to build with `yarn run build:forge`; the Forge version is slightly different from the Alloy version.

Also, note well that the build has sub-folders. Don't neglect to `cp -r` and to delete the subfolders before copying. 
