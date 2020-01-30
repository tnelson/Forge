import { View } from './view';
import { TreeLayout } from '../../layouts/tree-layout';
export class TreeView extends View {
    constructor(selection) {
        super(selection);
        this._layout = new TreeLayout(selection.select('#tree'));
        this._instance = null;
        this._is_visible = false;
        window.addEventListener('resize', this._layout.resize.bind(this._layout));
    }
    set_instance(instance) {
        if (this._is_visible) {
            this._layout.set_instance(instance);
        }
        else {
            this._instance = instance;
        }
    }
    _on_show() {
        this._is_visible = true;
        if (this._instance !== null) {
            this._layout.set_instance(this._instance);
            this._instance = null;
        }
    }
    _on_hide() {
        this._is_visible = false;
    }
}
