
var nodes = json.nodes.map(function(name){
	return {
		data: {
			id: name,
			label: name,
			color: "#666"
		}
	}
});


// How to do projection? static projection is fine for now.
// It would be nice to be able to do a callback... How could I do this in alloy?
// OK so projection just takes out those types from the tuples, fine.
// But what's alloy doing with the other stuff?
// I think the per-atom stuff is showing you every cut-down tuple that involved that atom


// Problems with projection:
// the way alloy does projection sucks. So, to start, it makes sense; if you are projecting over
// Book you just remove the Book column from every tuple. fine.
// But then it does this State-like thing, where you can advance through the books.
// What does that mean? You would think it means, for Book1, show me every cut-down tuple that originally
// contained Book1. It doesn't. It means, show me every cut-down tuple that, IF it contained a Book,
// ONLY contained Book1. Ya, that's the case. Weird.
// so ask tim about that.

// for now: make it look better, label nodes, do highlighting on mouseover.
// and make the html better.
// how? hide that shit until its rendered, first of all. idk how to do that though....
// size it correctly, give it borders.
// make sure the graph is centered.

// first priority: make it look nice, transitions on edges.



	// var edges = Object.entries(json.relations).reduce(function(acc, val){
	// 	relation_
	// 	return acc.concat(val);
	// }, []);
	//
	// console.log(edges);

 // Maybe the different nodes should be grouped separately?
 // Also, to start, I should just use an intelligent organization layout. So probably
 // either spread or bilkent
 // try bilkent

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
		// control point step size
		// control point weight
		// avsdf layout?

		layout: {
			name: 'cose-bilkent'
			// rows: 6
		},

		userZoomingEnabled: false
		// layout: {
		// 	name: "cose"
		// }

	});


// The styling is based on "data(color)"
// So to change, the style, I just need to change the data.

	cy.edges().on("mouseover", function(evt){
		evt.target.data("color", "green");
	});

	cy.edges().on("mouseout", function(evt){
		evt.target.data("color", "#ccc");
	});
