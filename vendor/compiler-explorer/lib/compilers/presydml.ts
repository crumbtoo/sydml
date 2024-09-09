import path from 'path';

import fs from 'fs-extra';

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
import type {ParseFiltersAndOutputOptions} from '../../types/features/filters.interfaces.js';
import { PreliminaryCompilerInfo } from '../../types/compiler.interfaces.js';
import {BaseCompiler} from '../base-compiler.js';
import { CompilationEnvironment } from '../compilation-env.js';
import type {
    OptPipelineBackendOptions,
    OptPipelineOutput,
    OptPipelineResults,
    Pass,
} from '../types/compilation/opt-pipeline-output.interfaces.js';
// import {CompilationEnvironment} from '../compilation-env.js';
// import {logger} from '../logger.js';
// import {RacketPassDumpParser} from '../parsers/racket-pass-dump-parser.js';

export class PresydmlCompiler extends BaseCompiler {
    static get key() {
        return 'presydml';
    }

    constructor(info: PreliminaryCompilerInfo, env: CompilationEnvironment)
    {
        super(info, env);

        this.compiler.optPipeline = {
            groupName: 'presydml passes',
        };
    }

    override async processOptPipeline(
        output,
        filters: ParseFiltersAndOutputOptions,
        optPipelineOptions: OptPipelineBackendOptions,
        debugPatched?: boolean,
    ) {
        return this.llvmPassDumpParser.process(
            debugPatched ? output.stdout : output.stderr,
            filters,
            optPipelineOptions,
        );
    }

    override async generateOptPipeline(
        inputFilename: string,
        options: string[],
        filters: ParseFiltersAndOutputOptions,
        optPipelineOptions: OptPipelineBackendOptions,
    ): Promise<OptPipelineOutput | undefined>
    {
        const pipelineDir = await this.newTempDir();
        const inputFile = this.filename(inputFilename);
        const pipelineFile = path.join(pipelineDir, path.basename(inputFile));
        await fs.copyFile(inputFile, pipelineFile);
        const execOptions = this.getDefaultExecOptions();
        const output = await this.runCompiler(this.compiler.exe, options, pipelineFile, execOptions);
        const finalOutput: OptPipelineResults = {};
        return {
            result: finalOutput,
        }
    }
}
