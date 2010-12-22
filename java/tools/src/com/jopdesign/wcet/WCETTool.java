/*
 * This file is part of JOP, the Java Optimized Processor
 * see <http://www.jopdesign.com/>
 *
 * Copyright (C) 2010, Stefan Hepp (stefan@stefant.org).
 * Copyright (C) 2010, Benedikt Huber (benedikt.huber@gmail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.jopdesign.wcet;

import com.jopdesign.common.AppEventHandler;
import com.jopdesign.common.AppInfo;
import com.jopdesign.common.AppSetup;
import com.jopdesign.common.ClassInfo;
import com.jopdesign.common.EmptyTool;
import com.jopdesign.common.MethodInfo;
import com.jopdesign.common.code.CallGraph;
import com.jopdesign.common.code.CallString;
import com.jopdesign.common.code.ControlFlowGraph;
import com.jopdesign.common.code.ExecutionContext;
import com.jopdesign.common.config.Config;
import com.jopdesign.common.config.Config.BadConfigurationException;
import com.jopdesign.common.config.OptionGroup;
import com.jopdesign.common.misc.MethodNotFoundException;
import com.jopdesign.common.misc.MiscUtils;
import com.jopdesign.common.processormodel.JOPConfig;
import com.jopdesign.common.processormodel.ProcessorModel;
import com.jopdesign.dfa.DFATool;
import com.jopdesign.dfa.analyses.CallStringReceiverTypes;
import com.jopdesign.dfa.analyses.LoopBounds;
import com.jopdesign.dfa.framework.ContextMap;
import com.jopdesign.wcet.allocation.BlockAllocationModel;
import com.jopdesign.wcet.allocation.HandleAllocationModel;
import com.jopdesign.wcet.allocation.HeaderAllocationModel;
import com.jopdesign.wcet.allocation.ObjectAllocationModel;
import com.jopdesign.wcet.analysis.WcetCost;
import com.jopdesign.wcet.annotations.SourceAnnotationReader;
import com.jopdesign.wcet.ipet.IPETConfig;
import com.jopdesign.wcet.jop.JOPWcetModel;
import com.jopdesign.wcet.jop.LinkerInfo;
import com.jopdesign.wcet.jop.LinkerInfo.LinkInfo;
import com.jopdesign.wcet.report.Report;
import com.jopdesign.wcet.uppaal.UppAalConfig;
import org.apache.bcel.generic.InstructionHandle;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.Map;
import java.util.Set;

/**
 * @author Stefan Hepp (stefan@stefant.org)
 * @author Benedikt Huber (benedikt.huber@gmail.com)
 */
public class WCETTool extends EmptyTool<AppEventHandler> {

    private ProjectConfig projectConfig;

    private String projectName;

    private AppInfo appInfo;
    private CallGraph callGraph;
    private DFATool dfaTool;

    private boolean genWCETReport;
    private Report results;
    private WCETProcessorModel processor;
    private SourceAnnotationReader sourceAnnotations;
    private File resultRecord;
    private LinkerInfo linkerInfo;
    private boolean hasDfaResults;

    public WCETTool() {
    }

    @Override
    public String getToolVersion() {
        return "0.1";
    }

    @Override
    public void registerOptions(OptionGroup options) {

    }

    @SuppressWarnings({"LiteralAsArgToStringEquals"})
    @Override
    public void onSetupConfig(AppSetup setup) throws BadConfigurationException {
        appInfo = setup.getAppInfo();
        Config config = setup.getConfig();


        this.projectConfig =  config;
        this.projectName = projectConfig.getProjectName();

        {
            File outDir = projectConfig.getOutDir();
            Config.checkDir(outDir,true);
            File ilpDir = new File(outDir,"ilps");
            Config.checkDir(ilpDir, true);
        }

        if(projectConfig.doGenerateReport()) {
            try {
                this.results = new Report(projectConfig.getConfig(),
                                          this,
                                          projectConfig.getReportDir());
            } catch (IOException e) {
                throw new BadConfigurationException("Unable to initialize reports", e);
            }
            this.genWCETReport = true;
        } else {
            this.genWCETReport = false;
        }

        if(projectConfig.saveResults()) {
            this.resultRecord = new File(config.getOption(ProjectConfig.RESULT_FILE));
            if(! projectConfig.appendResults()) {
                recordMetric("problem",this.getProjectName());
                recordMetric("date",new Date());
            }
        }

        if (projectConfig.getProcessorName().equals("allocObjs")) {
            this.processor = new ObjectAllocationModel(this);
        } else if (projectConfig.getProcessorName().equals("allocHandles")) {
            this.processor = new HandleAllocationModel(this);
        } else if (projectConfig.getProcessorName().equals("allocHeaders")) {
            this.processor = new HeaderAllocationModel(this);
        } else if (projectConfig.getProcessorName().equals("allocBlocks")) {
            this.processor = new BlockAllocationModel(this);
        } else if(projectConfig.getProcessorName().equals("jamuth")) {
            this.processor = new JamuthWCETModel(this);
        } else {
            try {
                this.processor = new JOPWcetModel(this);
            } catch (IOException e) {
                throw new BadConfigurationException("Unable to initialize JopWcetModel", e);
            }
        }
    }

    @Override
    public void initialize(Config config) {
    }

    @Override
    public void run(Config config) {

    }

    public AppInfo getAppInfo() {
        return appInfo;
    }

    public ProcessorModel getProcessorModel() {
        return appInfo.getProcessorModel();
    }

    public WCETProcessorModel getWCETProcessorModel() {
        return processor;
    }

    public ProjectConfig getProjectConfig() {
        return projectConfig;
    }

    public Config getConfig() {
        return projectConfig.getConfig();
    }

    public CallGraph getCallGraph() {
        return callGraph;
    }

    public DFATool getDfaTool() {
        return dfaTool;
    }

    public void setDfaTool(DFATool dfaTool) {
        this.dfaTool = dfaTool;
    }

    public ClassInfo getTargetClass() {
        return appInfo.getClassInfo(projectConfig.getTargetClass());
    }

    public String getTargetName() {
        return MiscUtils.sanitizeFileName(projectConfig.getAppClassName()
                + "_" + projectConfig.getTargetMethodName());
    }

    public MethodInfo getTargetMethod() {
        try {
            return appInfo.getMethodInfo(projectConfig.getTargetClass(),
                    projectConfig.getTargetMethod());
        } catch (MethodNotFoundException e) {
            throw new AssertionError("Target method not found: "+e);
        }
    }

    public String getProjectName() {
        return this.projectName;
    }

    public boolean reportGenerationActive() {
        return this.genWCETReport ;
    }

    public void setGenerateWCETReport(boolean generateReport) {
        this.genWCETReport = generateReport;
    }

    public Report getReport() {
        return results;
    }

    public boolean doWriteReport() {
        return projectConfig.getReportDir() != null;
    }

    /**
     * Get link info for a given class
     * @param cli the class
     * @return the linker info
     */
    public LinkInfo getLinkInfo(ClassInfo cli) {
        return this.linkerInfo.getLinkInfo(cli);
    }

    public LinkerInfo getLinkerInfo() {
        return this.linkerInfo;
    }

    /**
     * Convenience delegator to get the flowgraph of the given method
     *
     * @param mi the method to get the CFG for
     * @return the CFG for the method.
     */
    public ControlFlowGraph getFlowGraph(MethodInfo mi) {
        return mi.isAbstract() ? null : mi.getCode().getControlFlowGraph();
    }

    /**
     * Convenience delegator to get the size of the given method
     */
    public int getSizeInWords(MethodInfo mi) {
        return this.getFlowGraph(mi).getNumberOfWords();
    }


    public void writeReport() throws Exception {
        this.results.addStat( "classpath", getAppInfo().getClassPath());
        this.results.addStat( "application", projectConfig.getAppClassName());
        this.results.addStat( "class", projectConfig.getTargetClass());
        this.results.addStat( "method", projectConfig.getTargetMethod());
        this.results.writeReport();
    }

    public File getOutDir(String sub) {
        File outDir = projectConfig.getOutDir();
        File subDir = new File(outDir,sub);
        if(! subDir.exists()) subDir.mkdir();
        return subDir;
    }

    public File getOutFile(String file) {
        return new File(projectConfig.getOutDir(),file);
    }

    /* FIXME: Slow, caching is missing */
    public int computeCyclomaticComplexity(MethodInfo m) {
        ControlFlowGraph g = getFlowGraph(m);
        int nLocal = g.getGraph().vertexSet().size();
        int eLocal = g.getGraph().edgeSet().size();
        int pLocal = g.getLoopBounds().size();
        int ccLocal = eLocal - nLocal + 2 * pLocal;
        int ccGlobal = 0;
        for(ExecutionContext n: this.getCallGraph().getReferencedMethods(m)) {
            MethodInfo impl = n.getMethodInfo();
            ccGlobal += 2 + computeCyclomaticComplexity(impl);
        }
        return ccLocal + ccGlobal;
    }

    /* recording for scripted evaluation */
    public void recordResult(WcetCost wcet, double timeDiff, double solverTime) {
        if(resultRecord == null) return;
        Config c = projectConfig.getConfig();
        recordCVS("wcet","ipet",wcet,timeDiff,solverTime,
                    c.getOption(JOPConfig.CACHE_IMPL),
                    c.getOption(JOPConfig.CACHE_SIZE_WORDS),
                    c.getOption(JOPConfig.CACHE_BLOCKS),
                    c.getOption(IPETConfig.STATIC_CACHE_APPROX),
                    c.getOption(IPETConfig.ASSUME_MISS_ONCE_ON_INVOKE));

    }

    public void recordResultUppaal(WcetCost wcet,
                                   double timeDiff, double searchtime,double solvertimemax) {
        if(resultRecord == null) return;
        Config c = projectConfig.getConfig();
        recordCVS("wcet","uppaal",wcet,timeDiff,searchtime,solvertimemax,
                c.getOption(JOPConfig.CACHE_IMPL),
                c.getOption(JOPConfig.CACHE_SIZE_WORDS),
                c.getOption(JOPConfig.CACHE_BLOCKS),
                c.getOption(UppAalConfig.UPPAAL_CACHE_APPROX),
                c.getOption(UppAalConfig.UPPAAL_COMPLEXITY_TRESHOLD),
                c.getOption(UppAalConfig.UPPAAL_COLLAPSE_LEAVES),
                c.getOption(UppAalConfig.UPPAAL_CONVEX_HULL),
                c.getOption(UppAalConfig.UPPAAL_TIGHT_BOUNDS),
                c.getOption(UppAalConfig.UPPAAL_PROGRESS_MEASURE),
                c.getOption(UppAalConfig.UPPAAL_SUPERGRAPH_TEMPLATE),
                c.getOption(UppAalConfig.UPPAAL_EMPTY_INITIAL_CACHE)
                );
    }

    public void recordSpecialResult(String metric, WcetCost cost) {
        if(resultRecord == null) return;
        if(projectConfig.appendResults()) return;
        recordCVS("metric",metric,cost);
    }

    public void recordMetric(String metric, Object... params) {
        if(resultRecord == null) return;
        if(projectConfig.appendResults()) return;
        recordCVS("metric",metric,null,params);
    }

    private void recordCVS(String key, String subkey, WcetCost cost, Object... params) {
        Object[] fixedCols = { key, subkey };
        try {
            FileWriter fw = new FileWriter(resultRecord,true);
            fw.write(MiscUtils.joinStrings(fixedCols, ";"));
            if(cost != null) {
                Object[] costCols = { cost.getCost(), cost.getNonCacheCost(),cost.getCacheCost() };
                fw.write(";");
                fw.write(MiscUtils.joinStrings(costCols, ";"));
            }
            if(params.length > 0) {
                fw.write(";");
                fw.write(MiscUtils.joinStrings(params, ";"));
            }
            fw.write("\n");
            fw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

	/* Data flow analysis
	 * ------------------
	 */
	public boolean doDataflowAnalysis() {
		return projectConfig.doDataflowAnalysis();
	}

	@SuppressWarnings("unchecked")
	public void dataflowAnalysis() {
		int callstringLength = (int)projectConfig.callstringLength();
		topLevelLogger.info("Receiver analysis");
		CallStringReceiverTypes recTys = new CallStringReceiverTypes(callstringLength);
		Map<InstructionHandle, ContextMap<CallString, Set<String>>> receiverResults =
			dfaTool.runAnalysis(recTys);

		dfaTool.setReceivers(receiverResults);
		appInfo.setReceivers(receiverResults, callstringLength);

		topLevelLogger.info("Loop bound analysis");
		LoopBounds dfaLoopBounds = new LoopBounds(callstringLength);
		dfaTool.runAnalysis(dfaLoopBounds);
		dfaTool.setLoopBounds(dfaLoopBounds);
		this.hasDfaResults = true;
	}
	/**
	 * Get the loop bounds found by dataflow analysis
	 */
	public LoopBounds getDfaLoopBounds() {
		if(! hasDfaResults) return null;
		return dfaTool.getLoopBounds();
	}

}
