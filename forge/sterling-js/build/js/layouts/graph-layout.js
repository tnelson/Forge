import { DagreLayout } from './graph-layout-algorithms/dagre-layout';
import { AlloyGraph } from '../graph/alloy-graph';
export class GraphLayout {
    constructor(selection) {
        this._svg = selection
            .style('user-select', 'none')
            .style('font-family', 'monospace');
        this._dagre = new DagreLayout(this._svg);
        this._graph = null;
    }
    resize() {
    }
    set_instance(instance, projections) {
        this._graph = new AlloyGraph(instance);
        if (projections)
            this._graph.projections(projections);
        this._dagre.layout(this._graph);
    }
    set_projections(projections) {
        if (this._graph)
            this._graph.projections(projections);
        this._dagre.layout(this._graph);
    }
    set_edge_font_size(size) {
        this._dagre.set_edge_text_size(size);
    }
    set_node_font_size(size) {
        this._dagre.set_node_text_size(size);
    }
}
