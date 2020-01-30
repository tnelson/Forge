import { TableLayoutPreferences } from './table-layout-preferences';
export class TableLayout {
    constructor(selection, preferences) {
        this._prefs = preferences ? preferences : new TableLayoutPreferences();
        this._signatures = selection
            .append('div')
            .attr('class', 'table-view');
        this._fields = selection
            .append('div')
            .attr('class', 'table-view');
        this._is_compact = false;
        this._show_builtins = false;
        this._show_emptys = false;
    }
    set_fields(fields) {
        // Sort fields
        this._sort_fields(fields);
        // Bind data
        let tables = this._fields
            .selectAll('table')
            .data(fields);
        // Remove old tables
        tables
            .exit()
            .remove();
        tables = tables
            .enter()
            .append('table')
            .merge(tables);
        let headers = tables
            .selectAll('thead')
            .data(d => [d]);
        headers
            .exit()
            .remove();
        headers = headers
            .enter()
            .append('thead')
            .merge(headers);
        let titles = headers
            .selectAll('.title')
            .data(d => [d]);
        titles
            .exit()
            .remove();
        titles
            .enter()
            .append('tr')
            .append('th')
            .attr('class', 'title')
            .merge(titles)
            .text(d => d.label());
        let cols = headers
            .selectAll('.columns')
            .data(d => [d]);
        cols
            .exit()
            .remove();
        cols = cols
            .enter()
            .append('tr')
            .attr('class', 'columns')
            .merge(cols);
        let ch = cols
            .selectAll('th')
            .data(d => d.types());
        ch
            .exit()
            .remove();
        ch
            .enter()
            .append('th')
            .merge(ch)
            .text(d => d.label());
        let bodys = tables
            .selectAll('tbody')
            .data(d => [d]);
        bodys
            .exit()
            .remove();
        bodys = bodys
            .enter()
            .append('tbody')
            .merge(bodys);
        let rows = bodys
            .selectAll('tr')
            .data(d => d.tuples());
        rows
            .exit()
            .remove();
        rows = rows
            .enter()
            .append('tr')
            .merge(rows);
        let cells = rows
            .selectAll('td')
            .data(d => d.atoms());
        cells
            .exit()
            .remove();
        cells
            .enter()
            .append('td')
            .merge(cells)
            .text(d => d.label());
        this._update_fields();
    }
    set_signatures(signatures) {
        this._sort_signatures(signatures);
        // Bind data
        let tables = this._signatures
            .selectAll('table')
            .data(signatures);
        // Remove old tables
        tables
            .exit()
            .remove();
        tables = tables
            .enter()
            .append('table')
            .merge(tables);
        let headers = tables
            .selectAll('th')
            .data(d => [d]);
        headers
            .exit()
            .remove();
        headers
            .enter()
            .append('thead')
            .append('tr')
            .append('th')
            .merge(headers)
            .text(d => d.label());
        let bodys = tables
            .selectAll('tbody')
            .data(d => [d]);
        bodys
            .exit()
            .remove();
        bodys = bodys
            .enter()
            .append('tbody')
            .merge(bodys);
        let rows = bodys
            .selectAll('tr')
            .data(d => d.atoms());
        rows
            .exit()
            .remove();
        rows = rows
            .enter()
            .append('tr')
            .merge(rows);
        let cells = rows
            .selectAll('td')
            .data(d => [d]);
        cells
            .exit()
            .remove();
        cells
            .enter()
            .append('td')
            .merge(cells)
            .text(d => d.label());
        this._update_signatures();
    }
    toggle_builtins() {
        this._show_builtins = !this._show_builtins;
        this._update();
        return this._show_builtins;
    }
    toggle_compact() {
        this._is_compact = !this._is_compact;
        this._update();
        return this._is_compact;
    }
    toggle_emptys() {
        this._show_emptys = !this._show_emptys;
        this._update();
        return this._show_emptys;
    }
    _sort_fields(fields) {
        return fields.sort((a, b) => {
            return b.label().toLowerCase() < a.label().toLowerCase() ? 1 : -1;
        });
    }
    _sort_signatures(signatures) {
        return signatures.sort((a, b) => {
            if (a.builtin() && !b.builtin())
                return 1;
            if (b.builtin() && !a.builtin())
                return -1;
            return b.label().toLowerCase() < a.label().toLowerCase() ? 1 : -1;
        });
    }
    _update() {
        this._update_fields();
        this._update_signatures();
    }
    _update_fields() {
        let bc = this._prefs.border_color, bgc = this._prefs.background_color, p = this._prefs.padding_normal, pc = this._prefs.padding_compact, tc = this._prefs.text_color;
        let showfld = d => {
            return this._show_emptys || d.tuples().length ? null : 'none';
        };
        this._fields
            .selectAll('table, td, th')
            .attr('align', 'center')
            .style('border', '1px solid ' + bc)
            .style('padding', this._is_compact ? pc : p)
            .style('color', tc);
        this._fields
            .selectAll('.title')
            .attr('colspan', d => d.size());
        this._fields
            .selectAll('table')
            .style('display', showfld)
            .selectAll('tbody')
            .selectAll('tr')
            .style('background-color', (d, i) => i % 2 === 0 ? bgc : null);
    }
    _update_signatures() {
        let bc = this._prefs.border_color, bcd = this._prefs.border_color_dim, bgc = this._prefs.background_color, bgcd = this._prefs.background_color_dim, p = this._prefs.padding_normal, pc = this._prefs.padding_compact, tc = this._prefs.text_color, tcd = this._prefs.text_color_dim;
        let showsig = d => {
            let bi = d.builtin(), em = d.atoms().length === 0, vi = (this._show_builtins || !bi) && (this._show_emptys || !em);
            return vi ? null : 'none';
        };
        this._signatures
            .selectAll('table')
            .style('border', d => '1px solid ' + (d.builtin() ? bcd : bc))
            .style('color', d => d.builtin() ? tcd : tc)
            .style('display', showsig);
        this._signatures
            .selectAll('th')
            .style('padding', this._is_compact ? pc : p);
        this._signatures
            .selectAll('table')
            .filter(d => !d.builtin())
            .selectAll('tbody')
            .selectAll('tr')
            .style('background-color', (d, i) => i % 2 === 0 ? bgc : null);
        this._signatures
            .selectAll('table')
            .filter(d => d.builtin())
            .selectAll('tbody')
            .selectAll('tr')
            .style('background-color', (d, i) => i % 2 === 0 ? bgcd : null);
        this._signatures
            .selectAll('td')
            .attr('align', 'center')
            .style('padding', this._is_compact ? pc : p);
    }
}
