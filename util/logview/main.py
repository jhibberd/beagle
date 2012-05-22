import random
import tornado.ioloop
import tornado.web

class MainHandler(tornado.web.RequestHandler):

    def get(self):

        # Get generation.
        generation = int(self.get_argument('generation'))

        # Load relevant log files into memory to produce stats on requested
        # generation.
        genotypes =         LogLoader.load_genotypes(generation)
        scores =            LogLoader.load_scores(generation)
        evolution =         LogLoader.load_evolution(generation)
        msgs =              LogLoader.load_msgs(generation)
        genotypes_next =    LogLoader.load_genotypes(generation+1)
        scores_next =       LogLoader.load_scores(generation+1)
        msgs_next =         LogLoader.load_msgs(generation+1)

        # Process the log data into a list of detailed evolutionary events.
        events = []
        for a, b, c in evolution:

            genotype_a, score_a, msgs_a = (genotypes[a], scores[a], msgs[a])
            genotype_b, score_b, msgs_b = (genotypes[b], scores[b], msgs[b])
            genotype_c, score_c, msgs_c =\
                (genotypes_next[c], scores_next[c], msgs_next[c])

            states_a, states_b, states_c =\
                LogProcessor.get_states(genotype_a, genotype_b, genotype_c)
            score_diff = LogProcessor.get_score_diff(score_a, score_b, score_c)

            events.append((
                (genotype_a, states_a, score_a, msgs_a),
                (genotype_b, states_b, score_b, msgs_b),
                (genotype_c, states_c, score_c, msgs_c),
                score_diff,
                ))

        # Format the processed log data for HTML rendering, then render.
        content = LogFormatter.fmt(events)
        self.render('generation.html', 
            content=content,
            generation=generation,
            )
    

class GeneState(object):
    """The state (color) used when rendering a gene."""
    On =        0
    Off =       1
    Mutant =    2


class LogFormatter(object):
    """Format processed log data for HTML rendering."""

    @classmethod
    def fmt(cls, events):
        return [cls._fmt_event(*x) for x in events]

    @classmethod
    def _fmt_event(cls, parent_a, parent_b, child, score_diff):

        # Unpack arguments. 
        genotype_a, states_a, score_a, msgs_a = parent_a
        genotype_b, states_b, score_b, msgs_b = parent_b
        genotype_c, states_c, score_c, msgs_c = child

        # Convert the processed data to lines of HTML.
        longest_gene = cls._longest_gene(genotype_a, genotype_b, genotype_c)

        def to_line(genotype, states, score, msgs):
            genotype = cls._pad_genes(genotype, width=longest_gene)
            genotype = cls._add_state_tag(genotype, states)
            genotype = cls._join_genotype(genotype)
            msgs = cls._fmt_msgs(msgs)
            score = cls._pad_score(score)
            return cls._combine_all(genotype, score, msgs)

        line_a = to_line(genotype_a, states_a, score_a, msgs_a)
        line_b = to_line(genotype_b, states_b, score_b, msgs_b)
        line_c = to_line(genotype_c, states_c, score_c, msgs_c)
        line_d = cls._fmt_score_diff(score_diff)

        return cls._combine_lines(line_a, line_b, line_c, line_d)

    @staticmethod
    def _longest_gene(*genotypes):
        """Return the length (in chars) of the longest gene in a list of
        genotypes.
        """
        return max([max(map(len, g)) for g in genotypes])

    @staticmethod
    def _pad_genes(genotype, width):
        """Ensure all genes are rendered using the same number of chars so that
        gene 'columns' align.
        """
        return [g.ljust(width).replace(' ', '&nbsp;') for g in genotype]

    GENE_STATE_TO_HTML_TAG = {
        GeneState.On:       "gene-state-on",
        GeneState.Off:      "gene-state-off",
        GeneState.Mutant:   "gene-state-mutant",
        }
    @classmethod
    def _add_state_tag(cls, genotype, states):
        """Wrap each gene in its assigned state html tag."""
        def f(g, s):
            return "<{0}>{1}</{0}>".format(
                cls.GENE_STATE_TO_HTML_TAG[s], g)
        return [f(g, s) for g, s in zip(genotype, states)]

    @staticmethod
    def _join_genotype(genotype):
        """Join list of genes (genotype) into a single HTML string."""
        return '&nbsp'.join(genotype)

    SCORE_WIDTH = 14 # chars
    @classmethod
    def _pad_score(cls, s):
        """Ensure all scores are rendered using the same number of chars so 
        that score 'columns' align.
        """
        return str(s).ljust(cls.SCORE_WIDTH).replace(' ', '&nbsp;')

    @staticmethod
    def _fmt_msgs(msgs):
        """Format all genotype evaluations as an HTML list."""
        return "<ul class='msgs'>" + \
            ''.join(["<li>%s</li>" % x for x in msgs]) + "</ul>"

    @staticmethod
    def _combine_all(genotype, score, msgs):
        """Combine a formatted genotype, all its messages and its associated 
        score to a single HTML line.
        """
        return score + genotype + msgs

    @staticmethod
    def _combine_lines(*lines):
        """Combine all the formatted lines of an event into a single line of
        HTML.
        """
        return ''.join(lines) + "<br /><br />"

    @staticmethod
    def _fmt_score_diff(score_diff):
        if score_diff < 0:      css_class = "regression"
        elif score_diff > 0:    css_class = "progression"
        else:                   css_class = "stasis"
        score_diff = "%.6f" % score_diff # To 6 decimal places
        return "<evolved-score class='{0}'>{1}</evolved-score>".\
            format(css_class, score_diff)


class LogProcessor(object):
    """Calculate data derived from log data."""

    @staticmethod
    def get_states(parent_a, parent_b, child):
        """Returns a list of tuples representing the gene states for the parent
        and child genotypes.
        
        Returns:
            ((state_of_parent_a_gene_0, state_of_parent_a_gene_1, ...), 
             (state_of_parent_b_gene_0, state_of_parent_b_gene_1, ...),
             (state_of_child_gene_0, state_of_child_gene_1, ...))
        """
        def f(a, b, c):
            a_ = GeneState.On if a == c else GeneState.Off 
            b_ = GeneState.On if b == c else GeneState.Off 
            c_ = GeneState.On if c in [a,b] else GeneState.Mutant
            if a_ == GeneState.On and b_ == GeneState.On:
                a_, b_ = (
                    (GeneState.Off, GeneState.On), 
                    (GeneState.On, GeneState.Off),
                    )[random.randint(0,1)]
            return (a_, b_, c_)    
        return zip(*[f(a, b, c) for a,b,c in zip(parent_a, parent_b, child)])

    @staticmethod
    def get_score_diff(score_a, score_b, score_c):
        """Return the difference between the best parent score and the evolved
        child's score.

        A negative number indicates an evolutionary regression, where the child
        performed more poorly than the best parent.
        """
        return min(score_a, score_b) - score_c


class LogLoader(object):
    """Parse log files and load into memory."""

    @classmethod
    def load_genotypes(cls, generation):
        """Load human-friendly genotype strings and their associated hashes.

        Format:
            hash,genotype
        """
        d = {}
        for ln in cls._open_log('genotype', generation):
            hash_, genotype = ln[:-1].split(',', 1)
            genotype = genotype[1:-1].split(',')
            d[hash_] = genotype
        return d 

    @classmethod
    def load_scores(cls, generation):
        """Load scores achieved by each genotype.

        Format:
            hash,score
        """
        d = {}
        for ln in cls._open_log('score', generation):
            hash_, score = ln[:-1].split(',')
            d[hash_] = float(score)
        return d

    @classmethod
    def load_evolution(cls, generation):
        """Load genotype crossover data.

        Format:
            parent_a_hash,parent_b_hash,child_hash
        """
        xs = []
        for ln in cls._open_log('evolve', generation):
            triple = ln[:-1].split(',')
            xs.append(triple)
        return xs

    @classmethod
    def load_msgs(cls, generation):
        """Load the arbitrary messages logged for each genotype.

        Format:
            hash,msg
        """
        d = {}
        for ln in cls._open_log('msg', generation):
            hash_, msg = ln[:-1].split('|')
            d.setdefault(hash_, []).append(msg)
        return d

    @staticmethod
    def _open_log(file_type, generation):
        """Syntactic shortcut for getting handle to log file."""
        return open('/tmp/beagle/%s.%s' % (generation, file_type))


application = tornado.web.Application([
    (r"/", MainHandler),
], debug=True, template_path='html')

if __name__ == "__main__":
    application.listen(1831)
    tornado.ioloop.IOLoop.instance().start()

