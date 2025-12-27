# Load the XML document
[xml]$xml = Get-Content slftp.dproj

# Extract the UnitSearchPath
$unitSearchPath = $xml.Project.PropertyGroup.DCC_UnitSearchPath

# Replace backslashes with forward slashes
$unitSearchPath = $unitSearchPath -replace '\\', '/'

# Split the paths and filter out empty values and placeholders
$paths = $unitSearchPath.Split(';') | Where-Object { $_ -and $_ -ne '$(DCC_UnitSearchPath)' } | ForEach-Object { "-U$_" }

# Join the paths into a single string
$formattedPaths = $paths -join " "

# Output the result
$formattedPaths
