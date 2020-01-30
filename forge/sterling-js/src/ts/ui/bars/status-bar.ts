export class StatusBar {

    _statusbar;
    _command;
    _connection;

    constructor (selection) {

        this._statusbar = selection;
        this._command = selection.select('#command-status');
        this._connection = selection.select('#connection-status');

        this.set_command('None');
        this.set_connection_status('Disconnected');

    }

    set_command (command) {

        if (this._command) this._command.html('Command: ' + command);

    }

    set_connection_status (connection) {

        if (this._connection) this._connection.text('Status: ' + connection);

    }

}