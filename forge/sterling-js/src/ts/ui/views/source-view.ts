import * as d3 from 'd3';
import { View } from './view';
declare const hljs: any;

export class SourceView extends View {

    _tree;
    _view;
    _gutter;
    _editor;
    _message;
    _code;

    constructor (selection) {

        super(selection);
        this._tree = selection.select('.filetree');
        this._view = selection.select('.fileview');
        this._gutter = this._view.select('.gutter');
        this._editor = this._view.select('.editor');
        this._message = this._editor
            .append('div')
            .attr('class', 'message');
        this._code = this._editor
            .append('pre')
            .append('code')
            .attr('class', 'alloy');

        this._show_message('No files loaded.');

    }

    make_active (file) {

        if (file) {

            // Set the tree item to active
            this._tree
                .selectAll('.file')
                .classed('active', d => d === file);

            // Set the editor text
            this._code
                .html(file.text);

            // Highlight the code
            hljs.highlightBlock(this._code.node());

            // Update line numbers
            this._update_line_numbers(file);

        } else {

            this._code.text('Open a file.')

        }

    }

    set_files (files) {

        this._set_tree_data(files);
        this._show_code();

    }

    _on_show (): void {

    }

    _on_hide (): void {

    }

    _set_tree_data (files) {

        let fs = this._tree
            .selectAll('.file')
            .data(files, d => d.filename);

        // Remove old files
        fs.exit().remove();

        // Add new files
        let enter = fs.enter()
            .append('div')
            .attr('class', 'file')
            .on('click', d => this.make_active(d));

        // Add icon to new file
        enter.append('div')
            .attr('class', 'icon')
            .append('i')
            .attr('class', 'fas fa-file');

        // Add filename to new file
        enter.append('div')
            .attr('class', 'filename')
            .attr('id', d => d.filename)
            .text(d => d.filename);

        // Set the active file
        let active = this._tree
            .select('.active')
            .data();

        if (active.length) {
            this.make_active(active[0]);
        } else if (files.length) {
            this.make_active(files[0]);
        } else {
            this.make_active(null);
        }

    }

    _show_code () {

        this._message
            .style('display', 'none');

        this._code
            .style('display', null);

    }

    _show_message (message) {

        this._message
            .style('display', null)
            .text(message);

        this._code
            .style('display', 'none');

    }

    _update_line_numbers (file) {

        let lines = file.text.match(/\r?\n/g);
        let numlines = lines ? lines.length+1 : 2;

        let selection = this._gutter
            .selectAll('pre')
            .data(d3.range(numlines));

        selection
            .exit()
            .remove();

        selection
            .enter()
            .append('pre')
            .attr('class', 'line-number')
            .append('code')
            .append('span')
            .attr('class', 'hljs-comment')
            .html(d => d+1);

    }

}

hljs.registerLanguage('alloy', function () {

    let NUMBER_RE = '\\b\\d+';

    return {
        // case_insensitive
        case_insensitive: false,

        // keywords
        keywords: 'abstract all and as assert but check disj ' +
            'else exactly extends fact for fun iden iff implies ' +
            'in Int let lone module no none not one open or pred ' +
            'run set sig some sum univ',

        // contains
        contains: [

            // hljs.COMMENT
            hljs.COMMENT('//', '$'),
            hljs.COMMENT('--', '$'),
            hljs.COMMENT('/\\*', '\\*/'),

            {
                // className
                className: 'number',
                // begin
                begin: NUMBER_RE,
                // relevance
                relevance: 0
            }
        ]
    };
});
