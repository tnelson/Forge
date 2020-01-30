import { View } from './view';
import { GraphLayout } from '../../layouts/graph-layout';
import { ProjectionsBar } from '../bars/projections-bar';
export class GraphView extends View {
    constructor(selection) {
        super(selection);
        this._layout = new GraphLayout(selection.select('#graph'));
        this._instance = null;
        this._projections = null;
        this._is_visible = false;
        this._projections_bar = new ProjectionsBar(selection.select('#projections-bar'));
        window.addEventListener('resize', this._layout.resize.bind(this._layout));
        this._projections_bar.on_update(this._on_projections.bind(this));
    }
    set_instance(instance) {
        this._projections_bar.set_instance(instance);
        let projections = this._projections_bar.projections();
        if (this._is_visible) {
            this._layout.set_instance(instance, projections);
        }
        else {
            this._instance = instance;
            this._projections = projections;
        }
    }
    _on_projections(projections) {
        this._layout.set_projections(projections);
    }
    _on_show() {
        this._is_visible = true;
        if (this._instance !== null) {
            this._layout.set_instance(this._instance, this._projections);
            this._instance = null;
            this._projections = null;
        }
    }
    _on_hide() {
        this._is_visible = false;
    }
}
