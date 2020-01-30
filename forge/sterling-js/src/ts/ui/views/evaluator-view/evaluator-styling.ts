import * as d3 from 'd3';
import { EvaluatorStageNew } from './evaluator-stage-new';

export class EvaluatorStyling {

    _styling;
    _stage;

    _disconnectedNodes;
    _combineEdges;
    _showEdgeLabels;
    _showMiddles;
    _radius;
    _nodeFontSize;
    _edgeFontSize;
    _edgeLength;
    _sigColors;

    constructor (selection) {

        this._styling = selection;

        // LOCKING
        this._styling.append('button')
            .text('Lock all nodes')
            .on('click', () => {
                if (this._stage) this._stage.lockAllNodes();
            });
        this._styling.append('button')
            .text('Unlock all nodes')
            .on('click', () => {
                if (this._stage) this._stage.unlockAllNodes();
            });

        // DISCONNECTED NODES
        const disconnectedNodes = this._styling.append('div');
        this._disconnectedNodes = disconnectedNodes.append('input')
            .attr('type', 'checkbox')
            .attr('id', 'disconnectedNodes')
            .on('change', () => {
                if (this._stage) this._stage.toggleDisconnected();
                this._update();
            });
        disconnectedNodes.append('label')
            .attr('for', 'disconnectedNodes')
            .text('Show disconnected nodes');

        // COMBINED EDGES

        const combineEdges = this._styling.append('div');
        this._combineEdges = combineEdges.append('input')
            .attr('type', 'checkbox')
            .attr('id', 'combineEdges')
            .on('change', () => {
                if (this._stage) this._stage.toggleCombineEdges();
                this._update();
            });

        combineEdges.append('label')
            .attr('for', 'combineEdges')
            .text('Combine Edges');

        // SHOW EDGE LABELS
        const showEdgeLabels = this._styling.append('div');
        this._showEdgeLabels = showEdgeLabels.append('input')
            .attr('type', 'checkbox')
            .attr('id', 'showEdgeLabels')
            .on('change', () => {
                if (this._stage) this._stage.toggleEdgeLabels();
                this._update();
            });
        showEdgeLabels.append('label')
            .attr('for', 'showEdgeLabels')
            .text('Show edge labels');

        // SHOW MIDDLES

        const showMiddles = this._styling.append('div');
        this._showMiddles = showMiddles.append('input')
            .attr('type', 'checkbox')
            .attr('id', 'showMiddles')
            .on('change', () => {
                if (this._stage) this._stage.toggleShowMiddles();
                this._update();
            });

        showMiddles.append('label')
            .attr('for', 'showMiddles')
            .text('Show skipped atoms in edge labels');

        // CIRCLE RADIUS

        const radius = this._styling.append('div');
        this._radius = radius.append('input')
            .attr('type', 'number')
            .attr('id', 'radius')
            .attr('min', 1)
            .attr('max', 150)
            .on('change', () => {
                const radius = parseInt(this._radius.property('value'));
                if (this._stage) this._stage.setRadius(radius);
                this._update();
            });
        radius.append('label')
            .attr('for', 'radius')
            .text('Circle radius');

        // NODE FONT SIZE

        const nodefont = this._styling.append('div');
        this._nodeFontSize = nodefont.append('input')
            .attr('type', 'number')
            .attr('id', 'node-font')
            .attr('min', 1)
            .attr('max', 72)
            .on('change', () => {
                const size = parseInt(this._nodeFontSize.property('value'));
                if (this._stage) this._stage.setNodeFontSize(size);
                this._update();
            });
        nodefont.append('label')
            .attr('for', 'node-font')
            .text('Node font size');

        // EDGE FONT SIZE

        const edgefont = this._styling.append('div');
        this._edgeFontSize = edgefont.append('input')
            .attr('type', 'number')
            .attr('id', 'node-font')
            .attr('min', 1)
            .attr('max', 72)
            .on('change', () => {
                const size = parseInt(this._edgeFontSize.property('value'));
                if (this._stage) this._stage.setEdgeFontSize(size);
                this._update();
            });
        edgefont.append('label')
            .attr('for', 'node-font')
            .text('Edge font size');

        // TARGET EDGE LENGTH

        const edgeLength = this._styling.append('div');
        this._edgeLength = edgeLength.append('input')
            .attr('type', 'number')
            .attr('id', 'edge-length')
            .attr('min', 1)
            .attr('max', 1000)
            .on('change', () => {
                const size = parseInt(this._edgeLength.property('value'));
                if (this._stage) this._stage.setTargetEdgeLength(size);
                this._update();
            });
        edgeLength.append('label')
            .attr('for', 'edge-length')
            .text('Target edge length');

        // SIGNATURE COLORS
        this._sigColors = this._styling.append('div').attr('class', 'sigColors');
        this._updateSigColors();


    }

    setStage (stage: EvaluatorStageNew) {

        this._stage = stage;

        stage.addEventListener('colors', this._update.bind(this));

        this._update();

    }

    setVisible (visible: boolean) {

        this._styling.style('display', visible ? null : 'none');

    }

    _update () {

        this._disconnectedNodes.property('checked', this._stage._showDisconnected);
        this._combineEdges.property('checked', this._stage._combineEdges);
        this._showEdgeLabels.property('checked', this._stage._showEdgeLabels);
        this._showMiddles.property('checked', this._stage._showMiddles);
        this._radius.property('value', this._stage._radius);
        this._nodeFontSize.property('value', this._stage._nodeFontSize);
        this._edgeFontSize.property('value', this._stage._edgeFontSize);
        this._edgeLength.property('value', this._stage._forceLink.distance());
        this._updateSigColors();
    }

    _updateSigColors () {

        if (this._stage) {

            const strokes = this._stage._sigStrokes;
            const fills = this._stage._sigFills;
            const sigs = Array.from(strokes.keys()).map(sig => {
                return {
                    sig: sig,
                    stroke: strokes.get(sig),
                    fill: fills.get(sig)
                }
            });

            const parents = this._sigColors.selectAll('div.sigColor')
                .data(sigs)
                .join('div')
                .attr('class', 'sigColor');

            parents.selectAll('div.title')
                .data(d => [d])
                .join('div')
                .attr('class', 'title')
                .text(d => d.sig);

            const pickers = parents.selectAll('div.picker')
                .data(d => [{
                    type: 'Stroke',
                    color: d.stroke,
                    sig: d.sig
                }, {
                    type: 'Fill',
                    color: d.fill,
                    sig: d.sig
                }])
                .join('div')
                .attr('class', 'picker');

            pickers.selectAll('*').remove();
            pickers.append('input')
                .property('type', 'color')
                .property('value', d => d.color)
                .attr('id', d => d.type + d.sig)
                .on('change', (d, i, g) => {
                    const picker = d3.select(g[i]);
                    if (this._stage) {
                        if (d.type === 'Stroke') {
                            this._stage.setStrokeColor(d.sig, picker.property('value'));
                        }
                        if (d.type === 'Fill') {
                            this._stage.setFillColor(d.sig, picker.property('value'));
                        }
                    }
                });
            pickers.append('label')
                .attr('for', d => d.type + d.sig)
                .text(d => d.type);

        }

    }

}
