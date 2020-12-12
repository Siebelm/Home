cd "C:\Users\siebe\Documents\16_GitHub\siebelm.github.io\Bad_Banking_Behavior"

pygmentize -L styles

# Convert Jupyter file to html
jupyter nbconvert --to html "Model_Predictions.ipynb" --CSSHTMLHeaderPreprocessor.style=stata-dark 


# Original file
$orignal = "Model_Predictions.html"
# Output file
$index = "Model Predictions.html"

Remove-Item $index

# Addition
$in_header  = "in_header.html"

# Find where the last </title> tag is
if ((Select-String -Pattern "\</title\>" -Path $orignal |
    select -last 1) -match ":(\d+):")
{
    $insertPoint = $Matches[1]
    # Build up the output from the various parts
    Get-Content -Path $orignal | select -First $insertPoint | Out-File $index 
    Get-Content -Path $in_header | Out-File $index -Append
    Get-Content -Path $orignal | select -Skip $insertPoint | Out-File $index -Append
}

Copy-Item -Path $index -Destination $orignal

# Addition
$after_body = "after_body.html"

# Find where the last </body> tag is
if ((Select-String -Pattern "\</body\>" -Path $orignal |
    select -last 1) -match ":(\d+):")
{
    $insertPoint = $Matches[1]
    # Build up the output from the various parts
    Get-Content -Path $orignal | select -First $insertPoint | Out-File $index 
    Get-Content -Path $after_body | Out-File $index -Append
    Get-Content -Path $orignal | select -Skip $insertPoint | Out-File $index -Append
}

Remove-Item $orignal
