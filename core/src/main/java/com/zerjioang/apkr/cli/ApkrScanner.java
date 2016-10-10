package com.zerjioang.apkr.cli;

import apkr.external.modules.helpers.log4j.Log;
import apkr.external.modules.helpers.log4j.LoggerType;
import com.zerjioang.apkr.analysis.base.AbstractAndroidAnalysis;
import com.zerjioang.apkr.analysis.base.AnalysisFactory;
import com.zerjioang.apkr.exception.InvalidScanParametersException;
import com.zerjioang.apkr.sdk.helpers.ApkrConstants;
import com.zerjioang.apkr.sdk.model.base.APKFile;
import com.zerjioang.apkr.sdk.model.base.ApkrProject;
import com.zerjioang.apkr.temp.ApkrIntelligence;

import java.io.File;
import java.security.cert.CertificateException;

/**
 * Created by sergio on 3/9/16.
 */
public class ApkrScanner {

    public static final byte LOAD_VARIABLES = 0x0;
    private static boolean init = false;
    private ApkrProject project;

    public ApkrScanner(String[] args) throws InvalidScanParametersException {

        ApkrSettings settings = new ApkrSettings(args);

        //help info if requested
        if (settings.isHelpRequested()) {
            settings.showUsage();
            return;
        }
        else if(settings.getVersion()){
            System.out.println("Current version of apkr: "+ApkrConstants.ENGINE_VERSION);
            System.out.println("Check out on Github: https://github.com/apkr/apkr");
            System.out.println("Lead developer: @zerjioang");
            return;
        }
        else if(settings.hasFile()){
            //security check
            File file = settings.getInput();
            if (file!=null) {
                initScan(file);
            } else {
                throw new InvalidScanParametersException("Received parameters are not valid to launch the scan", args);
            }
        }
    }

    public static void main(String[] args) throws CertificateException, InvalidScanParametersException {
        new ApkrScanner(args);
    }

    public void stop() {
        //save report .json to file
        Log.write(LoggerType.TRACE, "Saving report file...");
        project.finish();
        Log.write(LoggerType.TRACE, "apkr scan finished");
    }

    private void initScan(File file) {
        //execute only once
        loadVariables();
        //read dex file from foldex x file y
        APKFile apk;

        Log.write(LoggerType.TRACE, "Reading .apk from local file");
        apk = new APKFile(file, APKFile.APKTOOL);

        Log.write(LoggerType.TRACE, "Building project");
        project = new ApkrProject(apk);

        Log.write(LoggerType.TRACE, "Running ApkrScan");

        Log.write(LoggerType.TRACE, "Project ID:\t" + project.getProjectId());

        AbstractAndroidAnalysis analyzer;
        analyzer = AnalysisFactory.getAnalyzer(AnalysisFactory.GENERAL);

        //Start analysis
        project.analyze(analyzer);
        //stop scan
        this.stop();
    }

    private void loadVariables() {
        if (!init) {
            //init data structs
            ApkrConstants.init();
            Log.write(LoggerType.TRACE, "Loading apkr data structs...");
            //create singleton instance of AtomIntelligence
            ApkrIntelligence.getInstance();
            Log.write(LoggerType.TRACE, "Data loaded!!");
            init = true;
        }
    }
}