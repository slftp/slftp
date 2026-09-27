<#
.SYNOPSIS
    Converts a DUnitX NUnit 2.x style XML test report to JUnit XML.

.DESCRIPTION
    GitLab CI test reports only understand the JUnit XML schema, but the
    DUnitX XML logger (TDUnitXXMLNUnitFileLogger) writes NUnit 2.x format.
    This script applies tests\nunit2junit.xslt to the report so the results
    show up in the GitLab pipeline test report.

    Never fails the build: any error is logged as a warning and the exit
    code stays 0 (it runs in after_script, where the input file may
    legitimately not exist, e.g. when the test build failed).

.PARAMETER InputFile
    Path to the NUnit XML report written by the DUnitX test runner.

.PARAMETER OutputFile
    Path of the JUnit XML report to write.
#>
param(
    [string]$InputFile = "tests\dunitx-results.xml",
    [string]$OutputFile = "tests\junit-results.xml"
)

try {
    if (-not (Test-Path $InputFile)) {
        Write-Warning "Input report '$InputFile' not found, skipping JUnit conversion."
        exit 0
    }
    $xsltPath = Join-Path $PSScriptRoot "nunit2junit.xslt"
    $xslt = New-Object System.Xml.Xsl.XslCompiledTransform
    $xslt.Load($xsltPath)
    $xslt.Transform((Resolve-Path $InputFile).Path, $OutputFile)
    Write-Host "Wrote JUnit report to '$OutputFile'."
} catch {
    Write-Warning "NUnit to JUnit conversion failed: $_"
}
exit 0
