import * as d3 from 'd3';

export class NavBar {

    _navbar;
    _active;
    _on_click_eval: Function;
    _on_click_graph: Function;
    _on_click_next: Function;
    _on_click_source: Function;
    _on_click_table: Function;
    _on_click_tree: Function;

    constructor (selection) {

        this._navbar = selection;

        selection.select('#nav-eval')
            .on('click', () => {
                if (this._on_click_eval) this._on_click_eval();
            });

        selection.select('#nav-graph')
            .on('click', () => {
                if (this._on_click_graph) this._on_click_graph();
            });

        selection.select('#nav-next')
            .on('click', () => {
                if (this._on_click_next) this._on_click_next();
            });

        selection.select('#nav-source')
            .on('click', () => {
                if (this._on_click_source) this._on_click_source();
            });

        selection.select('#nav-table')
            .on('click', () => {
                if (this._on_click_table) this._on_click_table();
            });

        selection.select('#nav-tree')
            .on('click', () => {
                if (this._on_click_tree) this._on_click_tree();
            });

    }

    on_eval (callback: Function): NavBar {
        this._on_click_eval = callback;
        return this;
    }

    on_graph (callback: Function): NavBar {
        this._on_click_graph = callback;
        return this;
    }

    on_next (callback: Function): NavBar {
        this._on_click_next = callback;
        return this;
    }

    on_source (callback: Function): NavBar {
        this._on_click_source = callback;
        return this;
    }

    on_table (callback: Function): NavBar {
        this._on_click_table = callback;
        return this;
    }

    on_tree (callback: Function): NavBar {
        this._on_click_tree = callback;
        return this;
    }

    set_eval_active () {
        this._make_active('nav-eval');
    }

    set_graph_active () {
        this._make_active('nav-graph');
    }

    set_source_active () {
        this._make_active('nav-source');
    }

    set_table_active () {
        this._make_active('nav-table');
    }

    set_tree_active () {
        this._make_active('nav-tree');
    }

    _make_active (selector) {
        this._active = selector;
        this._navbar
            .selectAll('.nav-icon-button')
            .classed('active', function () {
                return d3.select(this).attr('id') === selector;
            });

    }

}
