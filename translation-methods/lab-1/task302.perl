$start = 1;
$emptyLine = 0;
while (<>) {
    s/<[^>]*>//g;
    if (/^\s*$/) {
        if ($start == 0) {
            $emptyLine = 1
        }
    } else {
        $start = 0;
        s/^(\s+)|(\s+)$//g;
        s/(\s)(\s+)/ /g;
        if ($emptyLine) {
            print "\n";
            $emptyLine = 0;
        }
        print;
        print "\n";
    }
}
