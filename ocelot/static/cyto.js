
// need the edges to searate from each other, and be different colors

	console.log(edges);

	var nodes = json.nodes.map(function(name){
		return {data: {id: name, label: name}}
	});
	//
	// var edges = Object.entries(json.relations).reduce(function(acc, val){
	// 	relation_
	// 	return acc.concat(val);
	// }, []);
	//
	// console.log(edges);

	var edges = Object.entries(json.relations).reduce(function(acc, val){
		relation_name = val[0];
		console.log(relation_name);

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
