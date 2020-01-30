import * as d3 from 'd3';
export class EvaluatorOutput {
    constructor(selection) {
        this._output = selection;
    }
    deactivateAll() {
        this._output
            .selectAll('div.output')
            .classed('active', false);
    }
    expressions(expressions) {
        const selection = this._output
            .selectAll('div.output')
            .data(expressions, d => d.id)
            .join(enter => {
            const div = enter
                .append('div')
                .attr('class', 'output active');
            div.append('div').attr('class', 'expression');
            div.append('div').attr('class', 'result');
            return div;
        });
        selection
            .selectAll('div.expression')
            .each(renderExpression);
        selection
            .selectAll('div.result')
            .each(renderResult);
        this._scroll_down();
        function renderExpression(expression) {
            d3.select(this)
                .text(expression.expression);
        }
        function renderResult(expression) {
            d3.select(this)
                .classed('error', expression.error)
                .classed('expanded', true)
                .text(expression.result);
        }
    }
    _scroll_down() {
        this._output
            .property('scrollTop', this._output.property('scrollHeight'));
    }
}
