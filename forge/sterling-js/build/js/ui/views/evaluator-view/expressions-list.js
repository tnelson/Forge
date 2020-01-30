import { EventDispatcher } from '../../../util/event-dispatcher';
export class ExpressionsList extends EventDispatcher {
    constructor(selection) {
        super();
        this._expressionsList = selection;
        this._expressionsSet = new Set();
    }
    setExpressions(expressions) {
        this._expressionsList
            .selectAll('div')
            .data(expressions)
            .join('div')
            .attr('class', 'expression')
            .text(expr => expr)
            .on('click', d => {
            this.dispatchEvent({
                type: 'toggle',
                expression: d
            });
        });
    }
    setGraphedExpressions(expressions) {
        this._expressionsList
            .selectAll('div')
            .classed('graphed', expr => expressions.includes(expr));
    }
    setVisible(visible) {
        this._expressionsList.style('display', visible ? null : 'none');
    }
}
