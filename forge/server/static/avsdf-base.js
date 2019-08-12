(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("layout-base"));
	else if(typeof define === 'function' && define.amd)
		define(["layout-base"], factory);
	else if(typeof exports === 'object')
		exports["avsdfBase"] = factory(require("layout-base"));
	else
		root["avsdfBase"] = factory(root["layoutBase"]);
})(this, function(__WEBPACK_EXTERNAL_MODULE_0__) {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 6);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports) {

module.exports = __WEBPACK_EXTERNAL_MODULE_0__;

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


/**
 * This class implements data and functionality required for AVSDF layout per
 * circle.
 *
 *
 * Copyright: i-Vis Research Group, Bilkent University, 2007 - present
 */

var LGraph = __webpack_require__(0).LGraph;

// -----------------------------------------------------------------------------
// Section: Initializations
// -----------------------------------------------------------------------------

function AVSDFCircle(parent, graphMgr, vObject) {
    LGraph.call(this, parent, graphMgr, vObject);
    this.inOrder = undefined;
    this.currentIndex = 0;
    this.nodeSeparation = undefined;
    this.stack = [];
    this.perimeter = 0;
    this.centerX = 0;
    this.centerY = 0;
    this.radius = 0;
}

AVSDFCircle.prototype = Object.create(LGraph.prototype);

for (var property in LGraph) {
    AVSDFCircle[property] = LGraph[property];
}

AVSDFCircle.prototype.initOrdering = function () {
    this.inOrder = [];
};

// -----------------------------------------------------------------------------
// Section: Accessor Functions
// -----------------------------------------------------------------------------

// This function returns the array in which the nodes of this circle are kept in order.
AVSDFCircle.prototype.getOrder = function () {
    return this.inOrder;
};

// This function returns the x-coordinate of the center of this circle.
AVSDFCircle.prototype.getCenterX = function () {
    return this.centerX;
};

// This function returns the y-coordinate of the center of this circle.
AVSDFCircle.prototype.getCenterY = function () {
    return this.centerY;
};

// This function returns the radius of this circle.
AVSDFCircle.prototype.getRadius = function () {
    return this.radius;
};

// This function returns the total number of vertices of this circle.
AVSDFCircle.prototype.getSize = function () {
    return this.getNodes().length;
};

// This function calculates and returns the total number of crossings in this
// circle by adding up the crossing number of individual nodes on it.
AVSDFCircle.prototype.getTotalCrossingOfCircle = function () {
    var self = this;
    var crossingNumber = 0;

    for (var node in self.inOrder) {
        var nodeCrossing = node.getTotalCrossingOfEdges();
        if (nodeCrossing === -1) {
            return -1;
        }
        crossingNumber += nodeCrossing;
    }

    return crossingNumber / 4;
};

// This function checks whether or not all of the vertices of this circle are
// assigned an index on the circle.
AVSDFCircle.prototype.hasFinishedOrdering = function () {
    return this.currentIndex === this.getNodes().length;
};

// This function returns the node separation of this circle.
AVSDFCircle.prototype.getNodeSeparation = function () {
    return this.nodeSeparation;
};

// This function sets the node separation of this circle.
AVSDFCircle.prototype.setNodeSeparation = function (nodeSeparation) {
    this.nodeSeparation = nodeSeparation;
};

// -----------------------------------------------------------------------------
// Section: Remaining Functions
// -----------------------------------------------------------------------------

// This function traverses the vertices of this circle and corrects the angle
// of the vertices with respect to their circle indices.
AVSDFCircle.prototype.correctAngles = function () {
    var self = this;
    self.currentIndex = 0;

    self.inOrder.forEach(function (node) {
        return self.putInOrder(node);
    });
};

// This function puts the given node on the circle in the current order and
// sets its angle appropriately.
AVSDFCircle.prototype.putInOrder = function (node) {
    var self = this;

    var nodes = self.getNodes();

    // Note that id attribute of a node is added before
    // AVSDFLayout is called
    var found = false;
    for (var i = 0; i < nodes.length; i++) {
        if (nodes[i].id == node.id) {
            found = true;
            break;
        }
    }

    if (!found) {
        throw "The node must be a member of LGraph";
    }

    self.inOrder[self.currentIndex] = node;
    node.setIndex(self.currentIndex);

    if (self.currentIndex === 0) {
        node.setAngle(0.0);
    } else {
        node.setAngle(self.inOrder[self.currentIndex - 1].getAngle() + 2 * Math.PI * (node.getDiagonal() / 2 + self.nodeSeparation + self.inOrder[self.currentIndex - 1].getDiagonal() / 2) / self.perimeter);
    }

    self.currentIndex++;
};

// This function returns the next node to be placed on this circle with
// respect to the AVSDF algorithm.
AVSDFCircle.prototype.findNodeToPlace = function () {
    var self = this;
    var sDegreeNode = undefined;

    // Find the smallest degree vertex if the stack is empty
    if (self.stack.length === 0) {
        sDegreeNode = self.findUnorderedSmallestDegreeNode();
    }
    // Find the first vertex in the stack not yet placed
    else {
            var foundUnorderNode = false;

            while (!foundUnorderNode && !(self.stack.length === 0)) {
                sDegreeNode = self.stack.pop();
                foundUnorderNode = !sDegreeNode.isOrdered();
            }

            if (!foundUnorderNode) {
                sDegreeNode = undefined;
            }
        }

    // If no unordered vertex is found in the stack, find one
    // from the remaining ones
    if (sDegreeNode === undefined) {
        sDegreeNode = self.findUnorderedSmallestDegreeNode();
    }

    // Add the unordered neighbors of this node to the stack
    if (sDegreeNode !== undefined) {
        var neighbors = sDegreeNode.getNeighborsSortedByDegree();

        for (var i = neighbors.length - 1; i >= 0; i--) {
            var neighbor = neighbors[i];

            if (!neighbor.isOrdered()) // Check here for possible error
                {
                    self.stack.push(neighbor);
                }
        }
    }

    return sDegreeNode;
};

// This function calculates the radius of this circle with respect to the sizes
// of the vertices and the node separation parameter.
AVSDFCircle.prototype.calculateRadius = function () {
    var self = this;
    var totalDiagonal = 0;

    self.getNodes().forEach(function (node) {
        return totalDiagonal += Math.sqrt(node.getWidth() * node.getWidth() + node.getHeight() * node.getHeight());
    });

    self.perimeter = totalDiagonal + self.getNodes().length * self.nodeSeparation;
    var radius = self.perimeter / (2 * Math.PI);

    // Check here for possible error

    self.getParent().setWidth(2 * radius);
    self.getParent().setHeight(2 * radius);
    self.getParent().setCenter(self.getParent().getWidth(), self.getParent().getHeight());

    self.centerX = self.getParent().getCenterX();
    self.centerY = self.getParent().getCenterY();
    self.radius = self.getParent().getHeight() / 2;
};

// This function calculates the total number of crossings of all vertices of
// this circle.
AVSDFCircle.prototype.calculateEdgeCrossingsOfNodes = function () {
    this.getNodes().forEach(function (node) {
        return node.calculateTotalCrossing();
    });
};

// This function sets the index of each vertex to its position in inOrder
// array. Note that index of a node can be different from its place in the
// array due to crossing reduction phase of the AVSDF algorithm. It loads
// old index values to vertices due to an increase in the number of
// crossings with the new indices.
AVSDFCircle.prototype.loadOldIndicesOfNodes = function () {
    //this.inOrder.forEach( (node,index) => node.setIndex(index));
    var self = this;
    for (var i = 0; i < this.inOrder.length; i++) {
        self.inOrder[i].setIndex(i);
    }
};

// This function sets the position of each node in inOrder array to its index.
// Note that index of a node can be different from its place in the inOrder
// array due to crossing reduction phase of the AVSDF algorithm. This function
// puts the nodes to their new index values in inOrder array due to a
// decrease in the number of crossings with the new indices.
AVSDFCircle.prototype.reOrderVertices = function () {
    var self = this;
    this.getNodes().forEach(function (node) {
        return self.inOrder[node.getIndex()] = node;
    });
};

// This function finds and returns the unordered smallest degree vertex on
// this circle.
AVSDFCircle.prototype.findUnorderedSmallestDegreeNode = function () {
    var minDegree = Number.MAX_SAFE_INTEGER;
    var sDegreeNode = void 0;

    this.getNodes().forEach(function (node) {
        if (node.getDegree() < minDegree && !node.isOrdered()) {
            minDegree = node.getDegree();
            sDegreeNode = node;
        }
    });

    return sDegreeNode;
};

module.exports = AVSDFCircle;

/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var LayoutConstants = __webpack_require__(0).LayoutConstants;

function AVSDFConstants() {}

// AVSDFConstants inherits properties in LayoutConstants
for (var prop in LayoutConstants) {
    AVSDFConstants[prop] = LayoutConstants[prop];
}

AVSDFConstants.DEFAULT_NODE_SEPARATION = 60;

module.exports = AVSDFConstants;

/***/ }),
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


/**
 * This class implements data and functionality required for AVSDF layout per
 * edge.
 *

 * Copyright: i-Vis Research Group, Bilkent University, 2007 - present
 */

var LEdge = __webpack_require__(0).LEdge;

// -----------------------------------------------------------------------------
// Section: Initializations
// -----------------------------------------------------------------------------

function AVSDFEdge(source, target, vEdge) {
    LEdge.call(this, source, target, vEdge);
}

AVSDFEdge.prototype = Object.create(LEdge.prototype);

for (var properties in LEdge) {
    AVSDFEdge[properties] = LEdge[properties];
}

// -----------------------------------------------------------------------------
// Section: Accessor Functions
// -----------------------------------------------------------------------------

// The function getOtherEnd returns the other end of this edge.
AVSDFEdge.prototype.getOtherEnd = function (node) {
    return LEdge.prototype.getOtherEnd(node);
};

// -----------------------------------------------------------------------------
// Section: Remaining Functions
// -----------------------------------------------------------------------------

// This function checks whether this edge crosses with the input edge. It
// returns false, if any of the vertices those edges are incident to are not
// yet placed on the circle.
AVSDFEdge.prototype.crossesWithEdge = function (otherEdge) {
    var self = this;
    var sourcePos = self.getSource().getIndex();
    var targetPos = self.getTarget().getIndex();
    var otherSourcePos = otherEdge.getSource().getIndex();
    var otherTargetPos = otherEdge.getTarget().getIndex();

    // if any of the vertices those two edges are not yet placed
    if (sourcePos === -1 || targetPos === -1 || otherSourcePos === -1 || otherTargetPos === -1) {
        return false;
    }

    var otherSourceDist = otherEdge.getSource().getCircDistWithTheNode(self.getSource());
    var otherTargetDist = otherEdge.getTarget().getCircDistWithTheNode(self.getSource());
    var thisTargetDist = self.getTarget().getCircDistWithTheNode(self.getSource());

    if (thisTargetDist < Math.max(otherSourceDist, otherTargetDist) && thisTargetDist > Math.min(otherSourceDist, otherTargetDist) && otherTargetDist !== 0 && otherSourceDist !== 0) {
        return true;
    }

    return false;
};

// This function returns 1 if this edge crosses with the input edge, 0
// otherwise.
AVSDFEdge.prototype.crossingWithEdge = function (otherEdge) {
    var self = this;
    var result = self.crossesWithEdge(otherEdge);

    return result ? 1 : 0;
};

// This function calculates the total number of crossings of this edge with
// all the edges given in the input list.
AVSDFEdge.prototype.calculateTotalCrossingWithList = function (edgeList) {
    var self = this;
    var totalCrossing = 0;

    edgeList.forEach(function (edge) {
        return totalCrossing += self.crossingWithEdge(edge);
    });

    return totalCrossing;
};

module.exports = AVSDFEdge;

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


/**
 * This class implements data and functionality required for AVSDF layout per
 * node.
 *
 *
 * Copyright: i-Vis Research Group, Bilkent University, 2007 - present
 */

var LNode = __webpack_require__(0).LNode;
var Quicksort = __webpack_require__(0).Quicksort;

// -----------------------------------------------------------------------------
// Section: Initializations
// -----------------------------------------------------------------------------

function AVSDFNode(gm, vNode, loc, size) {
    // Constructor 1: AVSDFNode(gm, vNode, loc, size)
    if (loc !== undefined && size !== undefined) {
        LNode.call(this, gm, vNode, loc, size);
    }
    // Constructor 2: AVSDFNode(gm, vNode)
    else {
            LNode.call(this, gm, vNode);
        }

    // Angle of this node on the owner circle in radians
    this.angle = 0;

    // Index of this node on the owner circle
    this.circleIndex = -1;

    // Total number of crossings of the edges this node is incident to
    this.totalCrossingOfEdges = -1;

    // Whether the current edge crossing number is valid or it needs to be
    // recalculated
    this.isCrossingNumberValid = false;
}

AVSDFNode.prototype = Object.create(LNode.prototype);
for (var properties in LNode) {
    AVSDFNode[properties] = LNode[properties];
}

// -----------------------------------------------------------------------------
// Section: Accessor Functions
// -----------------------------------------------------------------------------

// This function returns the circle this node is owned by.
AVSDFNode.prototype.getCircle = function () {
    return this.getOwner();
};

// This function sets the index of this node on the circle, and sets the
// crossing number invalid. Due to the index change of the node; it needs to
// be recalculated.
AVSDFNode.prototype.setIndex = function (index) {
    this.circleIndex = index;
    this.isCrossingNumberValid = false;
};

// This function returns the index of this node in the ordering of its owner
// circle. Here -1 means that the vertex is not yet placed on its owner
//circle.
AVSDFNode.prototype.getIndex = function () {
    return this.circleIndex;
};

// This function returns the array of the neigbors of this node sorted in
// ascending order.
AVSDFNode.prototype.getNeighborsSortedByDegree = function () {
    var self = this;

    var result = Array.from(self.getNeighborsList());
    result = result.filter(function (node) {
        return node.getIndex() === -1;
    });

    result.sort(function (a, b) {
        return a.getDegree() - b.getDegree();
    });

    return result;
};

// This function returns the degree of this node.
AVSDFNode.prototype.getDegree = function () {
    return this.getEdges().length;
};

// This function returns whether or not this node is currently placed on its
// owner circle.
AVSDFNode.prototype.isOrdered = function () {
    return this.getIndex() > -1;
};

// This function sets the angle of this node w.r.t. its owner circle. Here
// the angle value is in radian.
AVSDFNode.prototype.setAngle = function (angle) {
    this.angle = angle;
};

// This function returns the angle of this node w.r.t. its owner circle. Here
// the angle value is in radian.
AVSDFNode.prototype.getAngle = function () {
    return this.angle;
};

// This function returns the index difference of this node with the input
// node. Note that the index difference cannot be negative if both nodes are
// placed on the circle. Here -1 means at least one of the nodes are not yet
// placed on the circle.
AVSDFNode.prototype.getCircDistWithTheNode = function (refNode) {
    var self = this;
    var otherIndex = refNode.getIndex();

    if (otherIndex === -1 || self.getIndex() === -1) {
        return -1;
    }

    var diff = self.getIndex() - otherIndex;

    if (diff < 0) {
        diff += self.getCircle().getSize();
    }

    return diff;
};

// This function finds the number of edge crossings between the edges of
// this node and the edges of the input one.
AVSDFNode.prototype.getCrossingNumberWithNode = function (otherNode) {
    var self = this;
    var totalCrossing = 0;

    self.getEdges().forEach(function (edge) {
        otherNode.getEdges().forEach(function (otherEdge) {
            totalCrossing += edge.crossingWithEdge(otherEdge);
        });
    });

    return totalCrossing;
};

// This function returns the total number of edge crossings. If the previously
// calculated value is not valid due to an index change on the circle, then
// a recalculation is performed.
AVSDFNode.prototype.getTotalCrossingOfEdges = function () {
    var self = this;

    if (!self.isCrossingNumberValid) {
        self.calculateTotalCrossing();
        self.isCrossingNumberValid = true;
    }

    return self.totalCrossingOfEdges;
};

// -----------------------------------------------------------------------------
// Section: Remaining Functions
// -----------------------------------------------------------------------------

// This function calculates the total number of crossings the edges of this
// node cause.
AVSDFNode.prototype.calculateTotalCrossing = function () {
    var self = this;
    var temp_crossing_count = 0;
    var temp_edge_list = [];
    temp_edge_list.push.apply(temp_edge_list, self.getCircle().getEdges());
    temp_edge_list = temp_edge_list.filter(function (ele) {
        return self.getEdges().indexOf(ele) < 0;
    });

    self.getEdges().forEach(function (edge) {
        return temp_crossing_count += edge.calculateTotalCrossingWithList(temp_edge_list);
    });

    self.totalCrossingOfEdges = temp_crossing_count;
};

module.exports = AVSDFNode;

/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


/**
 * This class implements the overall layout process for the AVSDF algorithm
 * (Circular Drawing Algorithm by He and Sykora).
 *
 *
 * Copyright: i-Vis Research Group, Bilkent University, 2007 - present
 */

var Layout = __webpack_require__(0).Layout;
var AVSDFConstants = __webpack_require__(2);
var AVSDFCircle = __webpack_require__(1);
var AVSDFNode = __webpack_require__(4);
var AVSDFEdge = __webpack_require__(3);

// -----------------------------------------------------------------------------
// Section: Initializations
// -----------------------------------------------------------------------------

// Constructor
function AVSDFLayout() {
    Layout.call(this);
    this.nodeSeparation = AVSDFConstants.DEFAULT_NODE_SEPARATION;
}

AVSDFLayout.prototype = Object.create(Layout.prototype);

for (var property in Layout) {
    AVSDFLayout[property] = Layout[property];
}

AVSDFLayout.prototype.newGraph = function (vObject) {
    this.avsdfCircle = new AVSDFCircle(null, this.graphManager, vObject);

    return this.avsdfCircle;
};

AVSDFLayout.prototype.newNode = function (vNode) {
    return new AVSDFNode(this.graphManager, vNode);
};

AVSDFLayout.prototype.newEdge = function (vEdge) {
    return new AVSDFEdge(null, null, vEdge);
};

// -----------------------------------------------------------------------------
// Section: Accessor Functions
// -----------------------------------------------------------------------------

// This function returns the position data for all nodes
AVSDFLayout.prototype.getPositionsData = function () {
    var allNodes = this.graphManager.getAllNodes();
    var pData = {};

    for (var i = 0; i < allNodes.length; i++) {
        var rect = allNodes[i].rect;
        var id = allNodes[i].id;

        pData[id] = {
            id: id,
            x: rect.getCenterX(),
            y: rect.getCenterY(),
            w: rect.width,
            h: rect.height
        };
    }

    return pData;
};

// -----------------------------------------------------------------------------
// Section: Layout Related
// -----------------------------------------------------------------------------

/**
 * This function performs layout on constructed l-level graph.
 * It returns true on success, false otherwise.
 * Important!: If you want to see the results of this function then, after this function is called, you have to calculate
 * and set the positions of every node. To do this call updateNodeCoordinates. However, updateNodeAngles on the other
 * hand is not needed (redundant) for this function.
 */
AVSDFLayout.prototype.layout = function () {
    var self = this;

    // Check if graph contains any compound structures
    if (self.graphManager.getGraphs().length > 1) {
        return false;
    }

    var clusterGraph = this.avsdfCircle; // Fixed reference, but now it is a bit redundant

    clusterGraph.setNodeSeparation(this.nodeSeparation);
    clusterGraph.calculateRadius();
    clusterGraph.initOrdering();

    while (!clusterGraph.hasFinishedOrdering()) {
        var node = clusterGraph.findNodeToPlace();
        clusterGraph.putInOrder(node);
    }

    return true;
};

// This function updates the angle (in radians) property of AVSDFNode elements in the circle
AVSDFLayout.prototype.updateNodeAngles = function () {
    this.graphManager.getRoot().correctAngles(); //AVSDFCircle object
};

// This function traverses the vertices of the graph and sets their correct coordinates with respect to the owner circle.
AVSDFLayout.prototype.updateNodeCoordinates = function () {
    var clusterGraph = this.graphManager.getRoot();

    clusterGraph.getNodes().forEach(function (node) {
        node.setCenter(clusterGraph.getCenterX() + clusterGraph.getRadius() * Math.cos(node.getAngle()), clusterGraph.getCenterY() + clusterGraph.getRadius() * Math.sin(node.getAngle()));
    });
};

// -----------------------------------------------------------------------------
// Section: Post Processing
// -----------------------------------------------------------------------------

/**
 * This method implements the post processing step of the algorithm, which
 * tries to minimize the number of edges further with respect to the local
 * adjusting algorithm described by He and Sykora.
 */
AVSDFLayout.prototype.initPostProcess = function () {
    this.avsdfCircle.calculateEdgeCrossingsOfNodes();

    var list = this.avsdfCircle.getNodes();

    list.sort(function (a, b) {
        return b.getTotalCrossingOfEdges() - a.getTotalCrossingOfEdges();
    });

    return list;
};

AVSDFLayout.prototype.oneStepPostProcess = function (node) {
    var self = this;

    var currentCrossingNumber = node.getTotalCrossingOfEdges();
    var newCrossingNumber = void 0;

    var neighbours = Array.from(node.getNeighborsList());

    for (var j = 0; j < neighbours.length; j++) {
        var neighbour = neighbours[j];

        var oldIndex = node.getIndex();
        var newIndex = (neighbour.getIndex() + 1) % self.avsdfCircle.getSize();

        if (oldIndex !== newIndex) {
            node.setIndex(newIndex);

            if (oldIndex < node.getIndex()) {
                oldIndex += self.avsdfCircle.getSize();
            }

            var index = node.getIndex();

            while (index < oldIndex) {
                var temp = self.avsdfCircle.getOrder()[index % self.avsdfCircle.getSize()];
                temp.setIndex((temp.getIndex() + 1) % self.avsdfCircle.getSize());
                index += 1;
            }

            node.calculateTotalCrossing();
            newCrossingNumber = node.getTotalCrossingOfEdges();

            if (newCrossingNumber >= currentCrossingNumber) {
                self.avsdfCircle.loadOldIndicesOfNodes();
            } else {
                self.avsdfCircle.reOrderVertices();
                currentCrossingNumber = newCrossingNumber;
            }
        }
    }
};

module.exports = AVSDFLayout;

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var avsdfBase = {};

avsdfBase.layoutBase = __webpack_require__(0);
avsdfBase.AVSDFConstants = __webpack_require__(2);
avsdfBase.AVSDFEdge = __webpack_require__(3);
avsdfBase.AVSDFCircle = __webpack_require__(1);
avsdfBase.AVSDFLayout = __webpack_require__(5);
avsdfBase.AVSDFNode = __webpack_require__(4);

module.exports = avsdfBase;

/***/ })
/******/ ]);
});