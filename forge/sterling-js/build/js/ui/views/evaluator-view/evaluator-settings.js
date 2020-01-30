import { EventDispatcher } from '../../../util/event-dispatcher';
import { EvaluatorStyling } from './evaluator-styling';
import { ExpressionsList } from './expressions-list';
export class EvaluatorSettings extends EventDispatcher {
    constructor(selection) {
        super();
        this._expressions = new ExpressionsList(selection.select('#eval-expressions'));
        this._expressionsTab = selection.select('#expressions-button');
        this._styling = new EvaluatorStyling(selection.select('#eval-styles'));
        this._stylingTab = selection.select('#styling-button');
        this._expressions.addEventListener('toggle', event => this.dispatchEvent(event));
        this._expressionsTab.on('click', this.showExpressions.bind(this));
        this._stylingTab.on('click', this.showStyles.bind(this));
        this.showExpressions();
    }
    setExpressions(expressions) {
        this._expressions.setExpressions(expressions);
    }
    setGraphedExpressions(expressions) {
        this._expressions.setGraphedExpressions(expressions);
    }
    setStage(stage) {
        this._styling.setStage(stage);
    }
    showExpressions() {
        this._stylingTab.classed('active', false);
        this._expressionsTab.classed('active', true);
        this._styling.setVisible(false);
        this._expressions.setVisible(true);
    }
    showStyles() {
        this._stylingTab.classed('active', true);
        this._expressionsTab.classed('active', false);
        this._styling.setVisible(true);
        this._expressions.setVisible(false);
    }
}
