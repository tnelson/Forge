<style>
.showcase {
    display: flex;
    flex-direction: column;
    flex-wrap: wrap;
}   
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
    width: 60%;
}
.logoimg {
    color: gray;
    float: left;
    margin-top: -10px;
    /* border-style: solid; */
    width: 35%;
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

# Forge: A Tool and Language for Teaching Formal Methods

<div class="example">
  <div class="logoimg"><img src="./img/logo_trimmed.png"/></div> 
  <div class="viztext">
    <div class="viznote">Forge is a lightweight formal-methods tool, similar to <a href="https://alloytools.org">Alloy 6</a>, built with <b>teaching</b> in mind. Forge provides a progression of sub-languages that gradually build in expressive power to match students' experience and expertise. <br/><br/>
    <A href="https://forge-fm.github.io/forge-documentation/latest/getting-started/installation/">Installation and Documentation</A>
    </div>
  </div>
</div>

<!-- <img src="img/logo_trimmed.png" width=30%></img> -->


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

Edit Forge in your favorite text editor; we have added support in both Visual Studio Code and DrRacket. 

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

We suggest that users install via Git, rather than using Racket's package system. Our Github contains release tags that are useful for those who wish to use older versions of the software. Forge now uses [semantic versioning](https://semver.org); major version changes introduce breaking changes.

| Forge Major Version | Textbook Version | VSCode Extension |
| ------------------- | ---------------- | ---------------- |
| **5.0.0 ([docs](https://forge-fm.github.io/forge-documentation/5.0/))** |  [**Spring 2026**](https://forge-fm.github.io/book/2026/) | [forge-fm](https://marketplace.visualstudio.com/items?itemName=SiddharthaPrasad.forge-fm) |
|   4.2.1 ([docs](https://forge-fm.github.io/forge-documentation/4.2/))   |  [Spring 2025](https://forge-fm.github.io/book/2025/)     | [forge-language-server](https://marketplace.visualstudio.com/items?itemName=SiddharthaPrasad.forge-language-server) |

## Materials and Github

Forge was originally created for [CSCI 1710, "Logic for Systems"](https://csci1710.github.io/) at Brown University. The notes and materials (except for recordings, which we cannot release) are public and free to use. A [textbook draft](https://forge-fm.github.io/book/) is also available.

Forge is open source and [hosted on Github](http://github.com/tnelson/forge). 

## Example Models

Beyond the notes and documention, you can find many examples of Forge models in the [`forge/examples` folder](https://github.com/tnelson/Forge/tree/main/forge/examples) of the repository. See the [README](https://github.com/tnelson/Forge/blob/main/forge/examples/README.md) for an itemized list. We are adding to this collection regularly! A selection of these that we use for teaching includes:

### Sample Systems- and Datastructure-Oriented Models

* [Raft leader election](https://github.com/tnelson/Forge/blob/main/forge/examples/raft/leader.frg)
* [Binary Decision Diagrams](https://github.com/tnelson/Forge/blob/main/forge/examples/bdds/bdds.frg)
* [Prim's algorithm](https://github.com/tnelson/Forge/blob/main/forge/examples/prim/prim.frg)
* [Destination-based MAC-layer network forwarding](https://github.com/tnelson/Forge/blob/main/forge/examples/network/network.frg)
* [Basic crypto-protocol domain model](https://github.com/tnelson/Forge/blob/main/forge/domains/crypto/base.frg)

### Sample Mathematically-Oriented Models 

* [Boolean logic](https://github.com/tnelson/Forge/blob/main/forge/examples/basic/booleanLogic.frg)
* [Finite-trace linear temporal logic](https://github.com/tnelson/Forge/blob/main/forge/examples/ltlf/ltl_f.frg)
* [Conway's Game of Life](https://github.com/tnelson/Forge/blob/main/forge/examples/basic/gameOfLife.frg) 

## Our Papers 

To read more about our motivation and design, see our [OOPSLA 2024 paper](https://cs.brown.edu/~tbn/publications/forge-oopsla24.pdf). 

## Contact 

Got questions? Reach out to [`Tim_Nelson@brown.edu`](mailto:Tim_Nelson@brown.edu). 

## Thanks 

We are grateful for support from the U.S. National Science Foundation ([award #2208731](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2208731)) and Brown University.
