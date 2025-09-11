use strict;
use warnings;

my $file = 'slftp.inc';

# Read parameter and use an empty string if no parameter is given to set the commit ID empty
my $git_commit = shift @ARGV // '';

# Open the file for reading
open my $in, '<', $file or die "Cannot open '$file' for reading: $!";

# Open a temporary file for writing
open my $out, '>', "$file.tmp" or die "Cannot open temporary file for writing: $!";

while (<$in>) {
    # Replace the line if it matches the pattern
    s/SL_REV: string.*/SL_REV: string = '$git_commit';/g;
    print $out $_;
}

close $in;
close $out;

# Replace the original file with the modified file
rename "$file.tmp", $file or die "Cannot rename temporary file: $!";
