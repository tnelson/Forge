import { Atom, Instance, Signature } from '..';
import { DagreLayout } from './graph-layout-algorithms/dagre-layout';
import { AlloyGraph } from '../graph/alloy-graph';

export class GraphLayout {

    _svg;

    _graph: AlloyGraph;
    _dagre: DagreLayout;

    constructor (selection) {

        this._svg = selection
            .style('user-select', 'none')
            .style('font-family', 'monospace')
            .style('font-size', '10px');

        this._dagre = new DagreLayout(this._svg);
        this._graph = null;

    }

    resize () {

    }

    set_instance (instance: Instance, projections?: Map<Signature, Atom>) {

        this._graph = new AlloyGraph(instance);
        if (projections) this._graph.projections(projections);
        this._dagre.layout(this._graph);

    }

    set_projections (projections: Map<Signature, Atom>) {

        if (this._graph) this._graph.projections(projections);
        this._dagre.layout(this._graph);

    }

}
