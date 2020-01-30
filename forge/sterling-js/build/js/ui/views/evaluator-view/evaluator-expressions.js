import { EventDispatcher } from '../../../util/event-dispatcher';
export class EvaluatorExpressions extends EventDispatcher {
    constructor(selection) {
        super();
        this._expressionsList = selection;
    }
    setExpressions(expressions) {
        this._expressionsList
            .selectAll('div')
            .data(expressions)
            .join('div')
            .attr('class', 'expression')
            .text(expr => expr.expression);
    }
    setGraphedExpressions(expressions) {
        const graphed = new Set(expressions);
        this._expressionsList
            .selectAll('div')
            .classed('graphed', expr => graphed.has(expr));
    }
}
