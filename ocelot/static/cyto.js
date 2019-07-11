	var nodes = json.nodes.map(function(name){
		return {data: {id: name, label: name}}
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



	// var edges = Object.entries(json.relations).reduce(function(acc, val){
	// 	relation_
	// 	return acc.concat(val);
	// }, []);
	//
	// console.log(edges);

	var edges = Object.entries(json.relations).reduce(function(acc, val){
		relation_name = val[0];

		return acc.concat(val[1].map(function(tuple){
			console.assert(tuple.length >= 1);

			var start = tuple[0];
			var end = tuple[tuple.length - 1];

			var label = relation_name;
			if (tuple.length > 2){
				var between = tuple.slice(1, tuple.length - 1);
				label = relation_name + "[" + between.join("->") + "]";
			}

			return {
				data: {
					id: tuple.join(),
					source: start,
					target: end,
					label: label,
					arrow: "triangle"
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
					'background-color': '#666',
					'label': 'data(id)',
					'font-size': 10
				}
			},

			{
				selector: 'edge',
				style: {
					'width': 2,
					'line-color': '#ccc',
					'target-arrow-color': '#ccc',
					'target-arrow-shape': 'triangle',
					'curve-style': 'bezier',
					'label': 'data(label)',
					'text-wrap': 'wrap',
					'control-point-weight': 0.8,
					'control-point-step-size': 50,
					'font-size': 10,
					'color': '#ccc'
				}
			}
		],

		// gotta use bezier, definitely.
		// control point step size
		// control point weight
		// avsdf layout?

		layout: {
			name: 'circle'
			// rows: 6
		}

		// layout: {
		// 	name: "cose"
		// }

	});
