var nodes = json.nodes.map(function(name){
	return {
		data: {
			id: name,
			label: name,
			color: "#666"
		}
	}
});

var edges = Object.entries(json.relations).reduce(function(acc, val){
	relation_name = val[0];

	return acc.concat(val[1].map(function(tuple){
		console.assert(tuple.length >= 1);

		var start = tuple[0];
		var end = tuple[tuple.length - 1];

		var label = relation_name + "\n\n\u2060";
		if (tuple.length > 2){
			var between = tuple.slice(1, tuple.length - 1);
			label = relation_name + "[" + between.join("->") + "]\n\n.";
		}

		return {
			data: {
				id: tuple.join(),
				source: start,
				target: end,
				label: label,
				color: "#ccc"
			}
		}
	}));
}, []);

var cy = cytoscape({

	container: document.getElementById('cy'), // container to render in

	elements: nodes.concat(edges),

	style: [ // the stylesheet for the graph
		{
			selector: 'node',
			style: {
				'background-color': 'yellow',
				'label': 'data(id)',
				'font-size': 12,
				'color': 'data(color)',
				'shape': 'barrel',
				'width': 100,
				'border-color': 'black',
				'border-width': 2,
				'text-valign': 'center',
				'text-halign': 'center'
			}
		},

		{
			selector: 'edge',
			style: {
			 	'width': 3,
				'line-color': "data(color)",
				'target-arrow-color': 'data(color)',
				'target-arrow-shape': 'triangle',
				'curve-style': 'bezier',
				'label': 'data(label)',
				'text-wrap': 'wrap',
				'control-point-weight': 0.8,
				'control-point-step-size': 50,
				'font-size': 12,
				'color': 'data(color)',
				"edge-text-rotation": "autorotate"
			}
		}
	],

	// gotta use bezier, definitely.

	layout: {
		name: "concentric"
	},

	userZoomingEnabled: false
});

cy.edges().on("mouseover", function(evt){
	evt.target.data("color", "green");
});

cy.edges().on("mouseout", function(evt){
	evt.target.data("color", "#ccc");
});
