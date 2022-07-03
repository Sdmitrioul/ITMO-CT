sub uniq {
    my %seen;
    grep !$seen{$_}++, @_;
}

my @arr = ();

while (<>) {
 if (/<a(.*?)href(.*?)=(.*?)(\"|')(((.*?):\/\/)|)(.*?)(\/|:|\"|')(.*)/) {
    my $tmp = $8;
    if ($tmp =~ /.+\.[^.]+/) {
      push(@arr, $tmp);
    }
 }
}

my @filtered = uniq(@arr);
my @result = sort @filtered;

print "$_\n" for (@result);