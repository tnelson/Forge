import * as d3 from 'd3';
import { EventDispatcher } from '../../../util/event-dispatcher';
import { Expression, Tuple } from './evaluator';

export interface Node {
    id: string,
    sigs: string[],
    stroke?: string,
    fill?: string,
    fixed?: boolean
}

export interface TupleSet {
    source: string,
    target: string,
    relations: {
        relation: string,
        middle: string[]
    }[]
}

interface Link {
    source: Node,
    target: Node,
    labels: string[]
}

export class EvaluatorStageNew extends EventDispatcher {

    _stage = null;
    _canvas = null;
    _context = null;

    _width = 0;
    _height = 0;

    _simulation = d3.forceSimulation<any>();
    _forceLink = d3.forceLink<any, any>().id(d => d.id);
    _forceCenter = d3.forceCenter();
    _forceCharge = d3.forceManyBody();

    _radius = 30;
    _nodeFontSize: number = 12;
    _edgeFontSize: number = 12;
    _defaultFill: string = '#ffffff';
    _defaultStroke: string = '#111111';

    _combineEdges: boolean = false;
    _showMiddles: boolean = true;
    _showEdgeLabels: boolean = true;
    _showDisconnected: boolean = true;

    _expressions: Expression[] = [];
    _links: Link[] = [];

    _nodes: Node[] = [];    // all nodes
    _disconnected = [];     // nodes not part of simulation
    _connected = [];        // nodes part of simulation

    _sigFills: Map<string, string> = new Map();
    _sigStrokes: Map<string, string> = new Map();

    constructor (selection) {

        super();

        this._stage = selection;
        this._canvas = selection.append('canvas');
        this._context = this._canvas.node().getContext('2d');

        this._simulation
            .force('link', this._forceLink)
            .force('center', this._forceCenter)
            .force('charge', this._forceCharge)
            .on('tick', this._repaint.bind(this));

        this._canvas.call(d3.drag()
            .container(this._canvas.node())
            .subject(this._dragSubject.bind(this))
            .on('start', this._dragStarted.bind(this))
            .on('drag', this._dragged.bind(this))
            .on('end', this._dragEnded.bind(this)));

        this._canvas
            .on('click', this._onClick.bind(this))
            .on('dblclick', this._onDblClick.bind(this));

        this.resize();

        this._forceLink.distance(6 * this._radius);
        this._forceCenter.x(this._width / 2).y(this._height / 2);
        this._forceCharge.strength(-100);

    }

    public resize () {

        const styles = getComputedStyle(this._stage.node());
        this._width = parseInt(styles.getPropertyValue('width'));
        this._height = parseInt(styles.getPropertyValue('height'));
        this._canvas.attr('width', this._width);
        this._canvas.attr('height', this._height);

        this._forceCenter.x(this._width / 2).y(this._height / 2);

        this.setExpressions(this._expressions);

    }

    public lockAllNodes () {

        this._connected.filter(node => !node.fixed).forEach(toggleFixed);
        this._repaint();

    }

    public toggleCombineEdges () {

        this._combineEdges = !this._combineEdges;
        this.setExpressions(this._expressions);

    }

    public toggleDisconnected () {

        this._showDisconnected = !this._showDisconnected;
        this._repaint();

    }

    public toggleEdgeLabels () {

        this._showEdgeLabels = !this._showEdgeLabels;
        this._repaint();

    }

    public toggleShowMiddles () {

        this._showMiddles = !this._showMiddles;
        this.setExpressions(this._expressions);

    }

    public setEdgeFontSize (size: number) {

        this._edgeFontSize = size;
        this._repaint();

    }

    public setExpressions (expressions: Expression[]) {

        // const previouslyConnected = this._resetTuples();

        this._expressions = expressions;
        // this._calculateLinks(expressions, previouslyConnected);
        this._calculateLinks();

        arrange_rows(this._disconnected, this._width, this._height, this._radius);
        this._simulation.nodes(this._connected);
        this._forceLink.links(this._links);
        this._simulation.alpha(0.3).restart();

    }

    public setFillColor (sig: string, color: string) {

        this._sigFills.set(sig, color);
        this._calculateNodeColors();
        this._repaint();

    }

    public setNodeFontSize (size: number) {

        this._nodeFontSize = size;
        this._repaint();

    }

    public setNodes (nodes: Node[]) {

        // Assign a color to each signature
        const sigset = new Set<string>();
        nodes.forEach(node => node.sigs.forEach(sig => sigset.add(sig)));
        this._setSignatures(Array.from(sigset));

        // Save nodes
        this._nodes = nodes;
        this._resetTuples();

        // Arrange static nodes
        arrange_rows(this._disconnected, this._width, this._height, this._radius);

        // Assign colors to nodes
        this._calculateNodeColors();

        // Restart simulation
        this._forceLink.links([]);
        this._simulation.nodes(this._connected);

    }

    public setRadius (radius: number) {

        this._radius = radius;
        this._repaint();

    }

    public setStrokeColor (sig: string, color: string) {

        this._sigStrokes.set(sig, color);
        this._calculateNodeColors();
        this._repaint();

    }

    public setTargetEdgeLength (length: number) {

        this._forceLink.distance(length);
        this._simulation.alpha(0.3).restart();

    }

    public unlockAllNodes () {

        this._connected.filter(node => node.fixed).forEach(toggleFixed);
        this._simulation.alpha(0.3).restart();
        this._repaint();

    }

    private _calculateLinks () {

        const expressions = this._expressions;
        const links: Link[] = [];
        const connectedSet = new Set();

        expressions.forEach(expression => {

            expression.tuples.forEach(tuple => {

                const source = this._nodes.find(node => node.id === tuple.source);
                const target = this._nodes.find(node => node.id === tuple.target);
                const label = this._tupleLabel(tuple);

                // Check that source and target are valid nodes
                if (!source)
                    throw Error(`Tuple source node is not valid: ${tuple.source}`);
                if (!target)
                    throw Error(`Tuple target node is not valid: ${tuple.target}`);

                // Make the link
                if (this._combineEdges) {

                    // If the link for this tuple exists already, add to its label,
                    // otherwise create a new link.
                    const existing = links.find(link =>
                        link.source === source && link.target === target
                    );

                    if (existing) {

                        if (!existing.labels.includes(label))
                            existing.labels.push(label);


                    } else {

                        links.push({
                            source: source,
                            target: target,
                            labels: [label]
                        });

                    }

                } else {

                    links.push({
                        source: source,
                        target: target,
                        labels: [label]
                    });

                }

                // Add nodes to set of connected nodes
                connectedSet.add(source);
                connectedSet.add(target);

            });

        });

        this._links = links;
        this._connected = [];
        this._disconnected = [];

        this._nodes.forEach(node => {
            if (connectedSet.has(node))
                this._connected.push(node);
            else
                this._disconnected.push(node);
        });

        this._disconnected.forEach(node => {
            node.fixed = false;
            node.fx = null;
            node.fy = null;
        });

    }

    private _calculateNodeColors () {

        this._nodes.forEach(node => {

            const nsigs = node.sigs.length;

            if (nsigs < 1) {
                node.stroke = this._defaultStroke;
                node.fill = this._defaultFill;
            } else {
                const lowest = node.sigs[nsigs-1];
                node.stroke = this._sigStrokes.get(lowest);
                node.fill = this._sigFills.get(lowest);
            }

        });

        this.dispatchEvent({
            type: 'colors'
        });

    }

    private _dragSubject () {

        return this._simulation.find(d3.event.x, d3.event.y, this._radius);

    }

    private _dragStarted () {

        if (!d3.event.active) this._simulation.alphaTarget(0.3).restart();
        d3.event.subject.fx = d3.event.subject.x;
        d3.event.subject.fy = d3.event.subject.y;

    }

    private _dragged () {
        d3.event.subject.fx = d3.event.x;
        d3.event.subject.fy = d3.event.y;
    }

    private _dragEnded () {
        if (!d3.event.active) this._simulation.alphaTarget(0);
        if (!d3.event.subject.fixed) d3.event.subject.fx = null;
        if (!d3.event.subject.fixed) d3.event.subject.fy = null;
    }

    private _onClick () {

        if (d3.event.ctrlKey) {

            const [x, y] = d3.mouse(this._canvas.node());
            const node = this._simulation.find(x, y, this._radius);
            if (node) toggleFixed(node);

        }

    }

    private _onDblClick () {

        const [x, y] = d3.mouse(this._canvas.node());
        const node = this._simulation.find(x, y, this._radius);
        if (node) toggleFixed(node);

    }

    private _resetTuples (): string[] {

        const previouslyConnected = this._connected.map(node => node.id);
        this._links = [];
        this._connected = [];
        this._disconnected = this._nodes.slice().sort((a, b) => alphaSort(a.id, b.id));
        return previouslyConnected;

    }

    private _repaint () {

        const context = this._context;
        const radius = this._radius;

        // Clear the context
        context.clearRect(0, 0, this._width, this._height);

        // Draw links
        context.beginPath();
        this._links.forEach(tuple => drawLink(context, tuple));
        context.lineWidth = 1;
        context.strokeStyle = '#111';
        context.stroke();

        // Draw link labels
        if (this._showEdgeLabels) {
            context.fillStyle = '#111';
            context.font = `${this._edgeFontSize}px monospace`;
            context.textAlign = 'center';
            context.textBaseline = 'middle';
            this._links.forEach(tuple => drawLinkLabel(context, tuple));
        }

        // Draw arrowheads
        context.beginPath();
        this._links.forEach(tuple => drawArrow(context, tuple, radius));
        context.lineWidth = 1;
        context.fillStyle = '#111';
        context.fill();

        // Draw nodes
        if (this._showDisconnected) {
            this._nodes.forEach(node => drawNode(context, node, radius));
        } else {
            this._connected.forEach(node => drawNode(context, node, radius));
        }

        // Draw node labels
        context.fillStyle = '#111';
        context.font = `${this._nodeFontSize}px monospace`;
        context.textAlign = 'center';
        context.textBaseline = 'middle';
        context.lineWidth = 1;
        if (this._showDisconnected) {
            this._nodes.forEach(node => drawNodeLabel(context, node));
        } else {
            this._connected.forEach(node => drawNodeLabel(context, node));
        }

    }

    private _setSignatures (signatures: string[]) {

        const oldFills = this._sigFills;
        const oldStrokes = this._sigStrokes;
        this._sigFills = new Map();
        this._sigStrokes = new Map();

        oldFills.forEach((color, sig) => {
            this._sigFills.set(sig, color);
        });

        oldStrokes.forEach((color, sig) => {
            this._sigStrokes.set(sig, color);
        });

        signatures.forEach(sig => {
            if (!this._sigFills.has(sig)) {
                this._sigFills.set(sig, this._defaultFill);
            }
            if (!this._sigStrokes.has(sig)) {
                this._sigStrokes.set(sig, this._defaultStroke);
            }
        });

    }

    private _tupleLabel (tuple: Tuple): string {

        return this._showMiddles
            ? tuple.relation + (tuple.middle.length ? `[${tuple.middle.join(',')}]` : '')
            : tuple.relation

    }

}

function alphaSort (a: string, b: string) {

    let nameA = a.toUpperCase();
    let nameB = b.toUpperCase();
    if (nameA < nameB) {
        return -1;
    }
    if (nameA > nameB) {
        return 1;
    }
    return 0;

}

function arrange_rows (nodes, width, height, radius) {

    const padding = radius / 2;
    let x = radius + padding,
        y = radius + padding;

    nodes.forEach(node => {
        node.x = x;
        node.y = y;
        x += 2 * radius + padding;
        if (x > width - padding - radius) {
            x = radius + padding;
            y += 2 * radius + padding;
        }
    });

}


const TWOPI = 2 * Math.PI;
const PI6 = Math.PI / 6;

function drawArrow (context, link, radius) {
    const ng = Math.atan2(link.target.y - link.source.y, link.target.x - link.source.x);
    const x = link.target.x - radius * Math.cos(ng);
    const y = link.target.y - radius * Math.sin(ng);
    context.moveTo(x, y);
    context.lineTo(x - 10 * Math.cos(ng - PI6), y - 10 * Math.sin(ng - PI6));
    context.lineTo(x - 10 * Math.cos(ng + PI6), y - 10 * Math.sin(ng + PI6));
    context.closePath();
}

function drawLinkLabel (context, link) {
    const x = (link.source.x + link.target.x) / 2;
    const y = (link.source.y + link.target.y) / 2;
    context.moveTo(x, y);
    context.fillText(link.labels.join(', '), x, y);
}

function drawNodeLabel (context, node) {
    context.moveTo(node.x, node.y);
    context.fillText(node.id, node.x, node.y);
}

function drawLink (context, link) {
    context.moveTo(link.source.x, link.source.y);
    context.lineTo(link.target.x, link.target.y);
}

function drawNode (context, node, radius) {

    context.beginPath();
    context.lineWidth = node.fixed ? 3 : 1;
    context.strokeStyle = node.stroke;
    context.fillStyle = node.fill;
    context.moveTo(node.x + radius, node.y);
    context.arc(node.x, node.y, radius, 0, TWOPI);
    context.fill();
    context.stroke();

}



function toggleFixed (node) {

    if (node.fixed) {
        node.fixed = false;
        node.fx = null;
        node.fy = null;
    } else {
        node.fixed = true;
        node.fx = node.x;
        node.fy = node.y;
    }

}
