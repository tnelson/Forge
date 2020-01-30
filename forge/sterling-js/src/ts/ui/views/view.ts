export abstract class View {

    _view_selection;

    protected constructor (selection) {
        this._view_selection = selection;
    }

    show () {
        if (this._view_selection) this._view_selection.style('display', null);
        this._on_show();
    }

    hide () {
        if (this._view_selection) this._view_selection.style('display', 'none');
        this._on_hide();
    }

    abstract _on_show (): void;
    abstract _on_hide (): void;

}
