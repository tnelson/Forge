var sigs = {};

// json = {
// 	nodes: ["state0", "state1", "state2", "state3", "state4", "state5"],
// 	relations: {
// 		next: [["state0", "state2"],
// 				["state1", "state3"],
// 				["state2", "state5"],
// 				["state3", "state0"],
// 				["state4", "state1"]],
// 		state: [["state0"], ["state1"], ["state2"], ["state3"], ["state4"], ["state5"]]
// 	},
// 	sig_names: ["state"]
// }

json.sig_names.forEach(function(name){
	sigs[name] = json.relations[name].map(function(singleton){
		return singleton[0];
	});
});

var relation_colors = [
	"#003366",
	"#3366cc",
	"#33ccff",
	"#0000ff",
	"#6600cc",
	"#cc00cc",
	"#ff66ff",
	"#ff0000",
	"#ff9900",
	"#993333",
	"#996600",
	"#663300",
	"#ffcc00",
	"#999966",
	"#669900",
	"#00ff00",
	"#00ff99",
	"#669999",
];

var compound_colors = [
	"#66ff99",
	"#ffff99",
	"#ccff99",
	"#ffcc99",
	"#ff9999",
	"#ff99cc",
	"#ffccff",
	"#cc99ff",
	"#99ccff",
	"#66ccff",
	"#66ffff",
	"#99ffcc",
];

var parent = function(node_name){
	return Object.keys(sigs).filter(function(sig_name){
		return sigs[sig_name].includes(node_name);
	})[0];
};

var compound_color_index = 0;

var nodes = json.nodes.map(function(name){
	return {
		data: {
			id: name,
			label: name,
			parent: parent(name)
		}
	};
}).concat(json.sig_names.map(function(name){
	compound_color_index += 1;
	return {
		data: {
			id: name,
			label: "",
			color: compound_colors[compound_color_index % compound_colors.length]
		}
	};
}));

var relation_color_index = 0;

var edges = Object.entries(json.relations).reduce(function(acc, val){
	relation_name = val[0];

	if (val[1].length > 0 && val[1][0].length >= 2) {
		relation_color_index += 1;
	}

	return acc.concat(val[1].map(function(tuple){
		console.assert(tuple.length >= 1);

		var start = tuple[0];
		var end = tuple[tuple.length - 1];

		var label = relation_name + "\n\n\u2060";
		if (tuple.length > 2){
			var between = tuple.slice(1, tuple.length - 1);
			label = relation_name + "[" + between.join("->") + "]\n\n\u2060";
		}

		return {
			classes: [relation_name],

			data: {
				id: tuple.join(),
				source: start,
				target: end,
				label: label,
				base_color: relation_colors[relation_color_index % relation_colors.length],
			}
		}
	}));
}, []);

// --------------------------------------------------------------------------------------------------
// arrange the nodes in a grid inside each compound box. (high priority) (turns out its a bad idea)
// More highlighting possibilities! highlight over compound node, or over node.
// binary relations as attributes
// fake projection: like attributes
// if every atom in A connects to B0, just have an edge from the A compound box to atom B0 (r condensation)
// --------------------------------------------------------------------------------------------------

var cy = cytoscape({

	container: document.getElementById('cy'), // container to render in

	elements: nodes.concat(edges),

	style: [ // the stylesheet for the graph
		{
			selector: 'node',
			style: {
				'background-color': "#e6e6e6",
				'label': 'data(label)',
				'font-size': 20,
				'width': 'label',
				'padding': 10,
				'text-valign': 'center',
				'text-halign': 'center',
				'border-width': 1,
				'border-color': 'black',
				'min-zoomed-font-size': 10,
			}
		},
		{
			selector: '$node > node',
			style: {
				'background-color': 'data(color)'
			}
		},
		{
			selector: 'edge',
			style: {
				'line-color': 'data(base_color)',
				'target-arrow-color': 'data(base_color)',
				'color': 'data(base_color)',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				'label': 'data(label)',
				'text-wrap': 'wrap',
				'control-point-weight': 0.6,
				'control-point-step-size': 100,
				'font-size': 16,
			 	'width': 3,
				'edge-text-rotation': 'autorotate',
				'z-compound-depth': 'auto',
				'min-zoomed-font-size': 10,
			}
		},
		{
			selector: 'edge.highlighted',
			style: {
				'line-color': 'black',
				'target-arrow-color': 'black',
				'color': 'black',
				'width': 6,
				'font-size': 20,
				'z-compound-depth': 'top'
			}
		},
		{
			selector: 'edge.faded',
			style: {
				'line-color': '#cccccc',
				'target-arrow-color': '#cccccc',
				'color': '#cccccc',

			}
		},
		{
			selector: 'node > node.highlighted',
			style: {
				'background-color': '#a6a6a6'
			}
		},
		{
			selector: '$node.highlighted > node',
			style: {
				'border-width': 3,
				'border-color': 'black',
			}
		},
	],

	userZoomingEnabled: false
});

function toggle_zoom(ele) {
	if (cy.userZoomingEnabled()){
		cy.userZoomingEnabled(false);
		ele.innerHTML = "Zoom DISABLED. Click to toggle."
	} else {
		cy.userZoomingEnabled(true);
		ele.innerHTML = "Zoom ENABLED. Click to toggle."
	}
}

cy.edges().on("mouseover", function(evt){
	cy.edges().not(evt.target).addClass('faded')
	evt.target.addClass('highlighted');
	evt.target.source().addClass('highlighted');
	evt.target.target().addClass('highlighted');
});

cy.edges().on("mouseout", function(evt){
	cy.edges().not(evt.target).removeClass('faded')
	evt.target.removeClass('highlighted');
	evt.target.source().removeClass('highlighted');
	evt.target.target().removeClass('highlighted');
});

var mouseOverChildren = function(children){
		children.addClass('highlighted');
		connectedEdges = children.connectedEdges();
		connectedNodes = connectedEdges.connectedNodes();
		connectedNodes.addClass('highlighted');
		connectedEdges.addClass('highlighted');
		cy.edges().not(connectedEdges).addClass('faded');
}

var mouseOutChildren = function(children){
	children.removeClass('highlighted');
	connectedEdges = children.connectedEdges();
	connectedNodes = connectedEdges.connectedNodes();
	connectedNodes.removeClass('highlighted');
	connectedEdges.removeClass('highlighted');
	cy.edges().not(connectedEdges).removeClass('faded');
}

cy.nodes().children().on('mouseover', function(evt){
	console.log("mouse over child")
	mouseOverChildren(evt.target);
});

cy.nodes().children().on('mouseout', function(evt){
	console.log("mouse out child")
	mouseOutChildren(evt.target);
});
//
cy.nodes().parents().on('mouseover', function(evt){
	if (evt.target.isParent()){
		console.log("mouse over parent")
		evt.target.addClass('highlighted');
		mouseOverChildren(evt.target.children());
		console.log(evt.target.connectedEdges());
	}
})
//
cy.nodes().parents().on('mouseout', function(evt){
	if (evt.target.isParent()){
		console.log("mouse out parent")
		evt.target.removeClass('highlighted');
		mouseOutChildren(evt.target.children());
	}
})

// whats hapepning is, edge.highlighted is applied AND edge.faded is applie,d at the same time. bad.

// mouse over parent, mouse out parent, mouse over child, mouse over parent.

// cy.nodes().children().on("mouseover", function(evt){
// 	// highlight node, incoming/outgoing edges
// });
//
// cy.nodes().parents().on("mouseover", function(evt){
// 	// add border, highlight child nodes, incoming/outgoing edges.
// })

// var colalayoutY = {
// 	name: "cola",
// 	flow: { axis: 'y', minSeparation: 100 },	// WAIT this DOES have an effect.
// 	// infinite: true
// }
var colalayout = {
	name: "cola",
	flow: { axis: 'y', minSeparation: 150 },	// WAIT this DOES have an effect.
	nodeSpacing: function(node){return 20;},
	animate: false,
	convergenceThreshold: 0.005
	// infinite: true
}

// var daglayout = {
// 	name: "dagre",
// 	rankDir: 'LR'
// }

// var ciselayout = {
// 	name: "cise",
// 	animate: false,
// 	clusters: Object.values(sigs),
// 	nodeRepulsion: 10,
// 	nodeSeparation: 5
// }

cy.layout(colalayout).run();
// FUCK ok cola doesn't work inside nodes. try dagre.


// savedEdges.restore();
// cy.layout(breadlayout).run();


// OK the breadthfirst layout, when it does work correctly, ALWAYS
// orients shit from top to bottom. I don't want that. I think I should roll my own.
// I think that's the solution.

// shit idk if I can roll my own actually. I mean I probably could,
// but there could be some complicated shit within a sig. like what if its circular.
// I could just drop down a row every time something tries to go back?
// nah, that wouldn't work well.

// Inside a sig, use cola, avsdf, or dagre.
// nah, avsdf puts shit in circles. I don't want that.
// either cola or dagre could be good. And if I use cola inside, I can use cola outside too.


// ah but cola outside doesn't apply cola inside. soooooo, still gotta do the bullshit with cola.
// cola DOES use the flow, remember to refresh.
// use dagre LR inside the parent nodes.

// breadthfirst is well and truly eliminated, as is klay.
// AGH I keep forgetting, you have to INCLUDE THE EDGES.

// Also, just stop trying to use the actual layouts to layout the subgraphs.
// It's never gonna work. They always have scenarios where they lay out vertically.
// that's what they're designed to do. They're not designed to ALWAYS make a horizontal graph.
// So do that part yourself.

// I changed the text size, that's why cise is now different.


// var layoutSubgraph = function(name, layout){
// 	var state = cy.$id(name);
// 	var children = state.children();
// 	var internalEdges = children.edgesWith(children);
// 	var subgraph = state.add(children).add(internalEdges);
// 	subgraph.layout(colalayoutX).run();
// }


// var savedElements = cy.elements();
//
// var state;
// var x;
// var y;
// var thisInternalEdges;
// var stateAndChildren;
// //
// state = cy.$id("state");
// // x = state.position().x;
// // y = state.position().y;
// thisInternalEdges = state.children().edgesWith(state.children());
// wholeShebang = state.add(state.children()).add(thisInternalEdges);
// cy.elements().remove();
// stateAndChildren.restore();
// thisInternalEdges.restore();
// stateAndChildren.layout(daglayout).run();

// state.position({x: x, y: y});


// name = "state";
// // json.sig_names.forEach(function(name){
// 	var state = cy.$id(name);
// 	var children = state.children();
// 	var internalEdges = children.edgesWith(children);
// 	var subgraph = state.add(children).add(internalEdges);
// 	subgraph.layout(colalayoutX).run();
// });

// savedElements.restore();

// state = cy.$id("Near");
// x = state.position().x;
// y = state.position().y;
// thisInternalEdges = state.children().edgesWith(state.children());
// state.children().add(thisInternalEdges).layout(breadlayout).run();
// state.position({x: x, y: y});

// state = cy.$id("goat");
// x = state.position().x;
// y = state.position().y;
// thisInternalEdges = state.children().edgesWith(state.children());
// state.children().add(thisInternalEdges).layout(breadlayout).run();
// state.position({x: x, y: y});

cy.resize();
cy.fit();
