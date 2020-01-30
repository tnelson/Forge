import * as d3 from 'd3';
import { EventDispatcher } from '../../../util/event-dispatcher';

export class EvaluatorInput extends EventDispatcher {

    _input;

    _curr = 0;
    _history: string[];

    constructor (selection) {

        super();

        this._input = selection;
        this._history = [];

        selection.on('keydown', this._onKeyDown.bind(this));

    }

    enable (enable: boolean) {

        this._input.attr('disabled', enable ? null : '');

    }

    _addToHistory (value: string) {

        this._history.push(value);
        this._curr = this._history.length;

    }

    _onDown () {

        if (this._curr < this._history.length) this._curr++;
        this._showCurrentHistorySelection();

    }

    _onEnter (ctrlKey: boolean) {

        const value = this._input.property('value');
        this._input.property('value', '');

        this._addToHistory(value);

        this.dispatchEvent({
            type: 'evaluate',
            text: value,
            ctrlKey: ctrlKey
        });

    }

    _onUp () {

        if (this._curr > 0) this._curr--;
        this._showCurrentHistorySelection();

    }

    private _showCurrentHistorySelection () {

        if (this._curr < this._history.length)
            this._input.property('value', this._history[this._curr]);
        else
            this._input.property('value', '');

    }

    private _onKeyDown () {

        const key = d3.event.key;

        if (key === 'Enter') {
            d3.event.preventDefault();
            this._onEnter(d3.event.ctrlKey);
        }
        else if (key === 'ArrowUp') {
            d3.event.preventDefault();
            this._onUp();
        }
        else if (key === 'ArrowDown') {
            d3.event.preventDefault();
            this._onDown();
        }

    }

}
