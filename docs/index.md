<style>
.showcase {
    display: flex;
    flex-direction: column;
    flex-wrap: wrap;
}   
/* .showcase>* {
  flex: 1 1 500px;
} */
.example {
    color: black;
    display: flex;
    align-items:center;
    margin-top: 10px;
}
.vizimg {
    color: gray;
    float: left;
    margin-top: 10px;
    border-style: solid;
    width: 50%;
}
.viztext {
    display: flex;
    flex-direction: column;
}
.vizlabel {
    color: black;
    float: left;
    font-weight: 500;
    font-size:large;
    margin-left: 10px;
}
.vizauthor {
    color: black;
    float: left;
    font-weight: 300;
    margin-left: 10px;
}
.viznote {
    color: black;
    float: left;
    /* font-weight: 300; */
    margin-left: 10px;
}
.vizpub {
    color: black;
    float: left;
    font-weight: 300;
    font-style:italic;
    margin-left: 10px;
}

</style>

# A Tool and Language for Teaching Formal Methods

Forge is a lightweight formal-methods tool, similar to [Alloy 6](https://alloytools.org), built with *teaching* in mind. Forge provides a progression of sub-languages that gradually build in expressive power to match students' experience and expertise. 

## Forge is for everyone...

<div class="showcase">

<div class="example">
  <div class="vizimg"><img src="./img/ttt_vscode.png"/></div> 
  <div class="viztext">
    <div class="viznote">from beginners...</div>
  </div>
</div>

<div class="example">
  <div class="vizimg"><img src="./img/crypto_vscode.png"/></div> 
  <div class="viztext">
    <div class="viznote">to domain experts...</div>
  </div>
</div>

<div class="example">
  <div class="vizimg"><img src="./img/abac_vscode.png"/></div> 
  <div class="viztext">
    <div class="viznote">to domain-specific language authors.</div>
  </div>
</div>
</div>

## Forge has...

### Modern Editor Integration

Edit Forge in your favorite text editor; we have added support in both [Visual Studio Code](https://github.com/csci1710/forge-language-extension-vscode/releases) and DrRacket. 

### Domain-Specific Visualization Support

Forge uses the [Sterling](https://sterling-js.github.io) visualizer to enable custom visualizations by _both students and instructors_.

<div class="showcase">

<div class="example">
  <div class="vizimg"><img src="./img/borrow-newt-custom.png"/></div>
  <div class="viztext">
    <div class="vizlabel">Rust Lifetimes and Borrowing</div>
    <div class="vizauthor">Thomas Castleman and Ria Rajesh</div>
    <div class="vizpub">(class project)</div>
  </div>
</div>

<div class="example">
  <div class="vizimg"><img src="./img/reflect-0-custom.png"/></div>
  <div class="viztext">
    <div class="vizlabel">Cryptographic Protocols</div>
    <div class="vizauthor">Abigail Siegel and Mia Santomauro</div>
    <div class="vizpub"><a href="https://cs.brown.edu/~tbn/publications/ssdnk-fest21-forge.pdf">(link to paper)</a></div>
  </div>
</div>

<div class="example">
  <div class="vizimg"><img src="./img/netlab-custom-def1.png"/></div>
  <div class="viztext">
    <div class="vizlabel">Network Reachability</div>
    <div class="vizauthor">Tim Nelson and Pamela Zave</div>
    <div class="vizpub"><a href="https://fm.csl.sri.com/SSFT23/">(link to lab)</a></div>
  </div>
</div>

</div>

## Getting Started 

To get started with Forge, follow [these instructions](https://csci1710.github.io/forge-documentation/getting-started/installation.html).

Forge was originally created for [CSCI 1710, "Logic for Systems"](https://csci1710.github.io/) at Brown University. The notes and materials are public and free to use. 

Forge is open source and [hosted on Github](http://github.com/tnelson/forge). 

## Contact 

Got questions? Reach out to `Tim_Nelson@brown.edu`. 

## Thanks 

We are grateful for support from the U.S. National Science foundation and Brown University.
