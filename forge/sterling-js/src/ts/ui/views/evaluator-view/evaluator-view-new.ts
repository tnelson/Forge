import Split from 'split.js';
import { Instance } from '../../..';
import { AlloyConnection } from '../../server/alloy-connection';
import { View } from '../view';
import { Evaluator, Expression } from './evaluator';
import { EvaluatorInput } from './evaluator-input';
import { EvaluatorOutput } from './evaluator-output';
import { EvaluatorSettings } from './evaluator-settings';
import { EvaluatorStageNew, Node } from './evaluator-stage-new';

export class EvaluatorViewNew extends View {

    _evaluator: Evaluator;
    _input: EvaluatorInput;
    _output: EvaluatorOutput;
    _stage: EvaluatorStageNew;
    _settings: EvaluatorSettings;

    _expressions: Map<string, Expression> = new Map();
    _expressionList: Expression[] = [];
    _graphedExpressions: Map<string, Expression> = new Map();

    _graphNextExpression: boolean = false;

    constructor (selection, alloy: AlloyConnection) {

        super(selection);

        Split(['#eval-editor', '#eval-display', '#eval-settings'], {
            sizes: [20, 60, 20],
            minSize: [200, 100, 200],
            gutterSize: 4,
            onDragEnd: () => { this._stage.resize(); }
        });
        Split(['#eval-output', '#eval-console'], {
            sizes: [75, 25],
            direction: 'vertical',
            gutterSize: 4
        });

        this._evaluator = new Evaluator(alloy);
        this._input = new EvaluatorInput(selection.select('#eval-input'));
        this._output = new EvaluatorOutput(selection.select('#eval-output'));
        this._stage = new EvaluatorStageNew(selection.select('#eval-display'));
        this._settings = new EvaluatorSettings(selection.select('#eval-settings'));
        this._settings.setStage(this._stage);

        this._input.addEventListener('evaluate', event => {
            this._evaluator.evaluate(event.text);
            this._graphNextExpression = event.ctrlKey;
        });

        this._evaluator.addEventListener('expression', event => {
            this._addExpression(event.expression);
        });

        this._settings.addEventListener('toggle', event => {
            const expression = event.expression;
            if (this._graphedExpressions.has(expression)) {
                this._graphedExpressions.delete(expression);
            } else {
                const expr = this._expressions.get(expression);
                this._graphedExpressions.set(expression, expr);
            }
            this._updateGraphedExpressions();
        });

    }

    set_instance (instance: Instance) {

        const nodes: Node[] = instance.atoms().map(atom => ({
            id: atom.id(),
            sigs: atom.signature().types().map(type => type.id())
        }));

        // Clear graphable expressions
        this._expressions = new Map();
        this._graphedExpressions = new Map();
        this._updateGraphedExpressions();

        // Set nodes and have evaluator extract relations from instance
        this._stage.setNodes(nodes);
        this._evaluator.setInstance(instance);

        // Make all previous expressions inactive
        this._output.deactivateAll();

    }

    _on_hide (): void {

    }

    _on_show (): void {

    }

    _addExpression (expression: Expression) {

        this._expressions.set(expression.expression, expression);
        this._expressionList.push(expression);

        this._output.expressions(this._expressionList.filter(expr => expr.result !== null));

        // Get expressions that have tuples
        const withTuples = Array.from(this._expressions.values())
            .filter(expr => expr.tuples.length)
            .map(expr => expr.expression);

        if (this._graphNextExpression) {

            this._graphedExpressions.set(expression.expression, expression);
            this._graphNextExpression = false;

        }

        this._settings.setExpressions(withTuples);
        this._updateGraphedExpressions();

    }

    _updateGraphedExpressions () {

        this._stage.setExpressions(Array.from(this._graphedExpressions.values()));
        this._settings.setGraphedExpressions(Array.from(this._graphedExpressions.keys()));

    }

}
