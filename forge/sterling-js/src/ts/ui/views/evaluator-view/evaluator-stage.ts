import * as d3 from 'd3';

export class EvaluatorStage {

    _stage = null;
    _svg = null;
    _type = 'graph';

    _nodes = [];
    _tuples = new Map();

    constructor (selection) {

        this._stage = selection;

    }

    public add_expression (expression) {

        // if (expression.error) return;
        //
        // const result = expression.result;
        // const re = /\{(.*)\}/g;
        // if (re.test(result)) {
        //
        //     if (result === '{}') return;
        //
        //     const raw_tuples = result.slice(1, -1).split(',');
        //     const tuples = raw_tuples
        //         .map(tuple => tuple.split('->')
        //             .map(atom => atom.trim()));
        //
        //     // Do stuff with new tuples here
        //
        //     this._render();
        //
        // }

    }

    public reset (nodes, tuples) {

        this._nodes = nodes || [];
        this._tuples = tuples || new Set();
        this._stage.selectAll('*').remove();
        this._render();

    }

    private _render () {

        const nodes = this._nodes;
        const tuples = this._tuples;

        const canvas = this._stage.append('canvas');
        const context = canvas.node().getContext('2d');
        const width = parseInt(canvas.style('width'));
        const height = parseInt(canvas.style('height'));

        const radius = 30;

        canvas.attr('width', width);
        canvas.attr('height', height);

        const atomset = new Set();
        tuples.forEach(tuple => tuple.forEach(atom => atomset.add(atom)));

        const links = [];
        this._tuples.forEach((relationset, key) => {
            const ids = key.split('->');
            const source = ids[0];
            const target = ids[1];
            const relations = Array.from(relationset);
            links.push({
                id: key,
                source: source,
                target: target,
                relations: relations
            });
        });

        const simulation = d3.forceSimulation<any>(nodes)
            .force('link', d3.forceLink<any, any>()
                .id(d => d.id)
                .links(links)
                .distance(6*radius))
            .force('charge', d3.forceManyBody().strength(-100))
            .force('center', d3.forceCenter(width / 2, height / 2))
            .on('tick', ticked);

        canvas
            .call(d3.drag()
                .container(canvas.node())
                .subject(dragsubject)
                .on('start', dragstarted)
                .on('drag', dragged)
                .on('end', dragended));

        function ticked () {

            context.clearRect(0, 0, width, height);

            // Draw links
            context.beginPath();
            links.forEach(drawLink);
            context.strokeStyle = '#111';
            context.stroke();

            // Draw nodes
            context.beginPath();
            nodes.forEach(drawNode);
            context.fillStyle = 'white';
            context.fill();
            context.strokeStyle = '#111';
            context.stroke();

            // Draw arrowheads
            context.beginPath();
            links.forEach(drawArrow);
            context.fillStyle = '#111';
            context.fill();

            // Draw node labels
            context.fillStyle = '#111';
            context.font = '12px monospace';
            context.textAlign = 'center';
            context.textBaseline = 'middle';
            nodes.forEach(drawLabel);

        }

        function dragsubject () {
            return simulation.find(d3.event.x, d3.event.y);
        }

        function dragstarted () {
            if (!d3.event.active) simulation.alphaTarget(0.3).restart();
            d3.event.subject.fx = d3.event.subject.x;
            d3.event.subject.fy = d3.event.subject.y;
        }

        function dragged () {
            d3.event.subject.fx = d3.event.x;
            d3.event.subject.fy = d3.event.y;
        }

        function dragended () {
            if (!d3.event.active) simulation.alphaTarget(0);
            d3.event.subject.fx = null;
            d3.event.subject.fy = null;
        }

        function drawLink (d) {
            context.moveTo(d.source.x, d.source.y);
            context.lineTo(d.target.x, d.target.y);
        }

        const PI6 = Math.PI / 6;

        function drawArrow (d) {
            const angle = Math.atan2(d.target.y - d.source.y, d.target.x - d.source.x);
            const x = d.target.x - radius * Math.cos(angle);
            const y = d.target.y - radius * Math.sin(angle);
            context.moveTo(x, y);
            context.lineTo(x - 10 * Math.cos(angle - PI6), y - 10 * Math.sin(angle - PI6));
            context.lineTo(x - 10 * Math.cos(angle + PI6), y - 10 * Math.sin(angle + PI6));
            context.closePath();
        }

        function drawNode (d) {
            context.moveTo(d.x + radius, d.y);
            context.arc(d.x, d.y, radius, 0, 2 * Math.PI);
        }

        function drawLabel (d) {
            context.moveTo(d.x, d.y);
            context.fillText(d.id, d.x, d.y);
        }


    }


}
