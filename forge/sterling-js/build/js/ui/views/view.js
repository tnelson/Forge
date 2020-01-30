export class View {
    constructor(selection) {
        this._view_selection = selection;
    }
    show() {
        if (this._view_selection)
            this._view_selection.style('display', null);
        this._on_show();
    }
    hide() {
        if (this._view_selection)
            this._view_selection.style('display', 'none');
        this._on_hide();
    }
}
