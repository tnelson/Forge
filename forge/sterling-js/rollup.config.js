import resolve from 'rollup-plugin-node-resolve';

export default {
    input: 'build/js/index',
    output: {
        file: 'dist/js/sterling.js',
        format: 'umd',
        name: 'sterling'
    },
    plugins: [resolve()],
    onwarn: (warning, warn) => {
        if (warning.code === 'CIRCULAR_DEPENDENCY') return;
        warn(warning);
    }
};