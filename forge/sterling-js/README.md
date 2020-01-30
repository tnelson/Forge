# sterling-js

This project contains all of the code for the web-based visualizer that is 
built in to the Sterling tool.

## Components and Directory Structure

We make use of the following tools:

* [Typescript](https://www.typescriptlang.org/): A typed superset of 
Javascript that compiles to plain Javascript.
* [Rollup](https://rollupjs.org/guide/en/): Module bundler which compiles all
 of our Javascript into a single file.
* [SASS](https://sass-lang.com/): Syntactically Awesome Stylesheets, a more 
straighforward way of developing stylesheets.

The project is laid out as follows.

* **build/**
  * **js/** - Directory containing the output Javascript from compiling 
  Typescript.  Never edit these files, as they will be overwritten.
* **dist/** - Directory containing all files needed for distribution.  The 
contents of this directory are distributed with the Sterling JAR file, and 
are what the end-user sees when they use the Sterling visualizer.
* **libs/** - Directory containing libraries that are not as easily used 
through npm, such as FontAwesome.
* **src/** - All source code for the project.
  * **scss/** - Stylesheets
  * **ts/** - Typescript
* **package.json** - npm package file, build scripts and project metadata
* **README.md** - This file
* **rollup.config.js** - Rollup configuration
* **tsconfig.json** - Typescript configuration

## Building

All javascript libraries are developed in Typescript.  Sources can be found 
in the ```src/ts``` directory.  All stylesheets are developed in SASS/SCSS, 
and sources can be found in the ```src/scss``` directory.

### Javascript 

Install Typescript using the command ```npm install -g typescript```. To 
compile all Typescript files into corresponding Javascript files, use the 
following command.

```bash
npm run build-ts
```

Note that the script runs in watch mode, so changes made to files during 
development are automatically compiled.  Javascript files are  output to the 
```build/js/``` directory.  To assemble all Javascript files into a single 
library for distribution, use the following command.

```bash
npm run build-js
```

Note that the script runs in watch mode, so changes  made to files (as a 
result of the Typescript recompiling) are automatically compiled.  The final
build is output to ```dist/js/sterling.js```

### Stylesheets

Install SASS using the command ```npm install -g sass```.  To compile all 
SCSS files into CSS, use the following command.

```bash
npm run build-css
```

Note that the script runs in watch mode, so changes made to styles are 
automatically compiled.  Styles are compiled into the 
```dist/css/index.css``` file.