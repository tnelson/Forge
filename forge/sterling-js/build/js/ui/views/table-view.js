import { View } from './view';
import { TableLayout } from '../../layouts/table-layout';
export class TableView extends View {
    constructor(selection) {
        super(selection);
        this._layout = new TableLayout(selection.select('#tables'));
        this._compact_button = selection.select('#table-compact-view');
        this._builtin_button = selection.select('#table-built-ins');
        this._empty_button = selection.select('#table-emptys');
        this._compact_button.on('click', this._on_toggle_compact.bind(this));
        this._builtin_button.on('click', this._on_toggle_builtin.bind(this));
        this._empty_button.on('click', this._on_toggle_empty.bind(this));
    }
    set_instance(instance) {
        this._layout.set_signatures(instance.signatures());
        this._layout.set_fields(instance.fields());
    }
    _on_show() {
    }
    _on_hide() {
    }
    _on_toggle_compact() {
        // Toggle state
        let is_compact = this._layout.toggle_compact();
        // Update the icon
        this._compact_button
            .select('i')
            .classed('fa-compress-arrows-alt', !is_compact)
            .classed('fa-expand-arrows-alt', is_compact);
        // Update the text
        this._compact_button
            .select('.text')
            .text(() => is_compact ? 'Normal View' : 'Compact View');
    }
    _on_toggle_builtin() {
        // Toggle state
        let show_builtins = this._layout.toggle_builtins();
        // Update the icon
        this._builtin_button
            .select('i')
            .classed('fa-eye-slash', show_builtins)
            .classed('fa-eye', !show_builtins);
        // Update text
        this._builtin_button
            .select('.text')
            .text(() => show_builtins ? 'Hide Built-in Signatures' : 'Show Built-in Signatures');
    }
    _on_toggle_empty() {
        // Toggle state
        let show_emptys = this._layout.toggle_emptys();
        // Update the icon
        this._empty_button
            .select('i')
            .classed('fa-eye-slash', show_emptys)
            .classed('fa-eye', !show_emptys);
        // Update text
        this._empty_button
            .select('.text')
            .text(() => show_emptys ? 'Hide Empty Tables' : 'Show Empty Tables');
    }
}
