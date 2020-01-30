import * as d3 from 'd3';
import Split from 'split.js';
import { View } from '../view';
import { EvaluatorStageNew } from './evaluator-stage-new';

export class EvaluatorView extends View {

    _alloy = null;
    _input = null;
    _output = null;
    _stage = null;
    _active = null;
    _nextid = 0;
    _expressions = [];

    _nodes = null;
    _tuples = null;

    constructor (selection) {

        super(selection);

        Split(['#eval-editor', '#eval-display'], {
            sizes: [30, 70],
            minSize: [300, 100],
            gutterSize: 4
        });
        Split(['#eval-output', '#eval-console'], {
            sizes: [75, 25],
            direction: 'vertical',
            gutterSize: 4
        });

        this._input = selection.select('#eval-input');
        this._output = selection.select('#eval-output');
        this._stage = new EvaluatorStageNew(selection.select('#eval-display'));

        this._initialize_input();

    }

    set_alloy (alloy) {

        if (alloy) {

            this._alloy = alloy;
            this._alloy.on_eval(this._parse_response.bind(this));

        }

    }

    set_instance (instance) {

        const nodes = instance.atoms().map(atom => ({
            id: atom.id()
        }));

        this._stage.nodes(nodes);

        const tuples = instance.tuples().map(tuple => {
            const atoms = tuple.atoms();
            return {
                source: atoms[0].id(),
                target: atoms[atoms.length-1].id(),
                relation: tuple.parent().id()
            };
        });

        this._stage.addTuples(tuples);

    }

    _add_error (message) {

        this._expressions.push({
            id: -1,
            expression: 'ERROR',
            result: message,
            active: false,
            error: true
        });

        this._update();

    }

    _clear () {

        this._expressions = [];
        this._active = null;
        this._update();

    }

    _disable () {

        this._input.attr('disabled', '');

    }

    _enable () {

        this._input.attr('disabled', null);

    }

    _expand_only (expression) {

        this._expressions.forEach(expr => {
            expr.expanded = expr === expression;
        });

    }

    _evaluate () {

        const input = this._input.property('value');
        this._input.property('value', '');

        const tmpres = this._alloy
            ? 'Evaluating...'
            : 'ERROR: No connection';

        if (input.length) {

            const expression = {
                id: this._nextid++,
                expression: input,
                result: tmpres,
                active: false,
                error: !this._alloy,
                expanded: true
            };

            this._expressions.push(expression);
            this._update();

            if (this._alloy) {
                this._alloy.request_eval(expression.id, expression.expression);
            }

        } else {

            this._enable();

        }

    }

    _initialize_input () {

        this._input.on('keydown', () => {
            if (d3.event.key === 'Enter') {
                d3.event.preventDefault();
                this._disable();
                this._evaluate();
            }
        });

    }

    _on_hide (): void {

    }

    _on_show (): void {

    }

    _parse_response (result: string) {

        const tokens = result.match(/EVL:(-?\d+):(.*)/);

        if (tokens === null) {

            this._add_error(`Invalid response:\n${result}`);

        } else {

            const id = parseInt(tokens[1]);
            const result = tokens[2];

            if (id === -1) {

                this._add_error(result);

            } else {

                const expr = this._expressions.find(expr => expr.id === id);

                if (expr) {

                    expr.result = result;
                    this._parse_result(expr);
                    this._expand_only(expr);
                    this._set_active(expr);

                } else {

                    this._add_error(`Unable to find expression ID: ${id}`);

                }

            }

        }

        this._update();
        this._enable();

    }

    _parse_result (expression) {

        const result = expression.result;
        if (result.slice(0, 4) === 'ERR:') {
            expression.result = result.slice(4);
            expression.error = true;
        }

    }

    _scroll_down () {

        this._output
            .property('scrollTop', this._output.property('scrollHeight'));

    }

    _set_active (expression) {

        this._active = expression;
        this._expressions.forEach(expr => {
            expr.active = expr === expression;
        });

    }

    _update () {

        const selection = this._output.selectAll('div.output')
            .data(this._expressions, d => d.id)
            .join('div')
            .attr('class', 'output')
            .classed('active', d => d.active);

        selection.selectAll('div')
            .data(d => [d, d])
            .join('div')
            .attr('class', (d, i) => {
                return i === 0
                    ? 'expression'
                    : 'result';
            })
            .classed('error', (d, i) => {
                return i === 1 && d.error
            })
            .classed('expanded', d => d.expanded)
            .text((d, i) => {
                return i === 0
                    ? d.expression
                    : d.result;
            });

        this._scroll_down();

    }

}
