use strict;
use warnings;

my $filename = 'December10/input';

open(FH, '<', $filename) or die $!;
my @lines = <FH>;
close(FH);

chomp @lines;
my @grid = map { [split //, $_] } @lines;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};
my $total_rating = 0;

my @directions = ([0, 1], [1, 0], [0, -1], [-1, 0]);

sub in_bounds {
    my ($x, $y) = @_;
    return $x >= 0 && $x < $cols && $y >= 0 && $y < $rows;
}

sub count_trails {
    my ($x, $y, $value, $visited, $path, $unique_paths) = @_;
    
    push @$path, "$x,$y";

    $visited->{"$x,$y"} = 1;

    my $is_leaf = 1;
    foreach my $dir (@directions) {
        my ($dx, $dy) = @$dir;
        my $nx = $x + $dx;
        my $ny = $y + $dy;

        if (in_bounds($nx, $ny) && !$visited->{"$nx,$ny"} && $grid[$ny][$nx] == $value + 1) {
            $is_leaf = 0;
            count_trails($nx, $ny, $value + 1, { %$visited }, [@$path], $unique_paths);
        }
    }

    if ($is_leaf) {
        my $path_str = join('->', @$path);
        $unique_paths->{$path_str} = 1;
    }
}

for (my $i = 0; $i < $rows; $i++) {
    for (my $j = 0; $j < $cols; $j++) {
        if ($grid[$i][$j] == 0) {
            my %unique_paths;
            count_trails($j, $i, 0, {}, [], \%unique_paths);
            $total_rating += scalar keys %unique_paths;
        }
    }
}

print "Total rating of all trailheads: $total_rating\n";
