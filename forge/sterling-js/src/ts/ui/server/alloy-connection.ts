import { Instance } from '../..';

export class AlloyConnection {

    _ws: WebSocket;

    _connected: boolean;
    _heartbeat_count: number;
    _heartbeat_id: number;
    _heartbeat_interval: number;
    _heartbeat_latency: DOMHighResTimeStamp;
    _heartbeat_timestamp: DOMHighResTimeStamp;

    _on_eval_cb: Function;
    _on_connected_cb: Function;
    _on_disconnected_cb: Function;
    _on_error_cb: Function;
    _on_instance_cb: Function;

    constructor () {

        this._ws = null;

        this._connected = false;
        this._heartbeat_count = 0;
        this._heartbeat_id = null;
        this._heartbeat_interval = 15000;
        this._heartbeat_latency = 0;
        this._heartbeat_timestamp = 0;

        this._on_connected_cb = null;
        this._on_disconnected_cb = null;
        this._on_error_cb = null;
        this._on_instance_cb = null;

    }

    average_latency (): number {

        if (this._heartbeat_count > 0) {
            return this._heartbeat_latency / this._heartbeat_count;
        }
        return 0;

    }

    connect () {

        if (this._ws) {
            this._ws.onclose = null;
            this._ws.close();
        }

        // For Forge:
        this._ws = new WebSocket('ws://localhost:' + window.location.search.slice(1));

        // For Alloy:
        // this._ws = new WebSocket('ws://' + location.hostname + ':' + location.port + '/alloy');

        this._ws.onopen = this._on_open.bind(this);
        this._ws.onclose = this._on_close.bind(this);
        this._ws.onerror = this._on_error.bind(this);
        this._ws.onmessage = this._on_message.bind(this);

    }

    connected () {

        return this._connected;

    }

    on_connected (cb: Function): AlloyConnection {

        this._on_connected_cb = cb;
        return this;
    }

    on_disconnected (cb: Function): AlloyConnection {

        this._on_disconnected_cb = cb;
        return this;
    }

    on_error (cb: Function): AlloyConnection {

        this._on_error_cb = cb;
        return this;

    }

    on_eval (cb: Function): AlloyConnection {

        this._on_eval_cb = cb;
        return this;

    }

    on_instance (cb: Function): AlloyConnection {

        this._on_instance_cb = cb;
        return this;

    }

    request_current (): AlloyConnection {

        if (this._ws) this._ws.send('current');
        return this;

    }

    request_eval (id: number, command: string): AlloyConnection {

        if (this._on_eval_cb) {

            if (this._ws) {

                this._ws.send('EVL:' + id + ':' + command);

            } else {

                this._on_eval_cb(`EVL:${id}:No connection.`);

            }
        }

        return this;
    }

    request_next (): AlloyConnection {

        if (this._ws) this._ws.send('next');
        return this;

    }

    _on_open (e: Event) {

        this._connected = true;
        this._reset_heartbeat();
        if (this._on_connected_cb) this._on_connected_cb();

    }

    _on_close (e: Event) {

        this._connected = false;
        this._ws = null;
        if (this._on_disconnected_cb) this._on_disconnected_cb();

    }

    _on_error (e: Event) {

        if (this._on_error_cb) this._on_error_cb(e);

    }

    _on_message (e: MessageEvent) {

        this._reset_heartbeat();
        let header = e.data.slice(0, 4);
        let data = e.data.slice(4);

        switch (header) {

            case 'pong':
                this._heartbeat_latency += performance.now() - this._heartbeat_timestamp;
                this._heartbeat_count += 1;
                break;

            case 'EVL:':
                if (this._on_eval_cb) this._on_eval_cb(e.data);
                break;

            case 'XML:':
                console.log(data);
                if (data.length) {
                    let instance = Instance.fromXML(data);
                    if (this._on_instance_cb) this._on_instance_cb(instance);
                }
                break;

            default:
                break;

        }

    }

    _reset_heartbeat () {

        clearTimeout(this._heartbeat_id);
        this._heartbeat_id = window.setTimeout(this._ping.bind(this), this._heartbeat_interval);

    }

    _ping () {

        if (this._ws) {
            this._heartbeat_timestamp = performance.now();
            this._ws.send('ping');
        }

    }

}
