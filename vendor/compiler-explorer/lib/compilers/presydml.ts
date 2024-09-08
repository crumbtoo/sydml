// import path from 'path';

// import fs from 'fs-extra';

// import type {
//     CompilationResult,
//     CompileChildLibraries,
//     ExecutionOptions,
// } from '../../types/compilation/compilation.interfaces.js';
// import type {
//     OptPipelineBackendOptions,
//     OptPipelineOutput,
// } from '../../types/compilation/opt-pipeline-output.interfaces.js';
// import type {PreliminaryCompilerInfo} from '../../types/compiler.interfaces.js';
// import type {ParseFiltersAndOutputOptions} from '../../types/features/filters.interfaces.js';
import {BaseCompiler} from '../base-compiler.js';
// import {CompilationEnvironment} from '../compilation-env.js';
// import {logger} from '../logger.js';
// import {RacketPassDumpParser} from '../parsers/racket-pass-dump-parser.js';

export class PresydmlCompiler extends BaseCompiler {
    static get key() {
        return 'presydml';
    }
}
