use File::Copy;

# Configure
$THRESHOLD = 7 * 24 * 60 * 60; # 7 days
$MYDIR     = "C:/Users/mburkhardt/Desktop";
$ARCHIVE   = "Desktop Debris";

# Set the working directory
chdir($MYDIR) or die("Directory does not exist");

# Check to see whether the archive directory exists
if ( ! -d $ARCHIVE ) {
  mkdir($ARCHIVE);
}

opendir($fh, ".") or die("Cannot open directory");
@list = readdir($fh) or die("Cannot read directory contents");
close($fh);

$now = time();

for $f ( @list ) {
  $last_mod_time = (stat ($f))[9];
  if ( ($now - $last_mod_time) > $THRESHOLD ) {
    move($f, "${ARCHIVE}/${f}");
  }
}

