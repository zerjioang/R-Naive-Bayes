package com.zerjioang.apkr.cli;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;

import java.io.File;

/**
 * Created by .local on 10/10/2016.
 */
public class ApkrSettings{

    private JCommander cmm;

    @Parameter(names = { "-v", "--verbose" }, description = "Be a verbose output")
    public boolean verbose = false;

    @Parameter(names = {"-d", "--debug" }, description = "Enable debug mode" )
    private boolean debug = false;

    @Parameter(names = {"-version" }, description = "Show current version of the engine")
    private boolean version;

    @Parameter(names = {"-f", "--file" }, description = "Input Android application to scan", required = true)
    private File input;

    @Parameter(names = {"-h", "--help" }, description = "show this help", help = true)
    private boolean help;

    public ApkrSettings(String[] args){
        cmm = new JCommander(this);
        if(args!=null && args.length>0){
            cmm.parse(args);
        }
    }

    public boolean getVersion() {
        return version;
    }

    public boolean isDebug() {
        return debug;
    }

    public File getInput() {
        return input;
    }

    public boolean isHelpRequested() {
        return help;
    }

    public void showUsage() {
        cmm.usage();
    }

    public boolean hasFile() {
        return getInput()!=null;
    }
}
