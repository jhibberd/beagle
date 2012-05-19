import random
import tornado.ioloop
import tornado.web

class GeneState(object):
    """The state (color) used when rendering a gene."""
    On =        0
    Off =       1
    Mutant =    2


class MainHandler(tornado.web.RequestHandler):

    def get(self):

        # Get generation.
        generation = int(self.get_argument('generation'))

        # Load log files for current generation into memory.
        genotypes = self.load_genotypes(generation)
        scores = self.load_scores(generation)
        evolution = self.load_evolution(generation)

        # Load log files for next generation into memory.
        genotypes_next = self.load_genotypes(generation+1)
        scores_next = self.load_scores(generation+1)

        # Processing...
        output = []
        lookup = lambda hash_: (genotypes[hash_], scores[hash_])
        lookup_next = lambda hash_: (genotypes_next[hash_], scores_next[hash_])
        for a, b, c in evolution:

            genotype_a, score_a = lookup(a)
            genotype_b, score_b = lookup(b)
            genotype_c, score_c = lookup_next(c)

            states = self.get_states(genotype_a, genotype_b, genotype_c)
            states_a, states_b, states_c = zip(*states)

            longest_gene = self.longest_gene([
                genotype_a, genotype_b, genotype_c])
            genotype_a = self.pad_genes(longest_gene, genotype_a)
            genotype_b = self.pad_genes(longest_gene, genotype_b)
            genotype_c = self.pad_genes(longest_gene, genotype_c)

            genotype_a = self.markup_ancestry(genotype_a, states_a) 
            genotype_b = self.markup_ancestry(genotype_b, states_b) 
            genotype_c = self.markup_ancestry(genotype_c, states_c) 

            genotype_a = self.join(genotype_a)
            genotype_b = self.join(genotype_b)
            genotype_c = self.join(genotype_c)
   
            score_diff = self.calc_child_score_diff(score_a, score_b, score_c)
            score_a = self.markup_score(score_a)
            score_b = self.markup_score(score_b)
            score_c = self.markup_score(score_c)

            output.append(
                ((genotype_a, score_a), 
                 (genotype_b, score_b),
                 (genotype_c, score_c),
                 score_diff))

        # Display all genotypes in the generation.
        self.render('generation.html', 
            content=output,
            generation=generation,
            )
    
    # TODO(jhibberd) Put these functions into separate classes.

    @staticmethod
    def calc_child_score_diff(score_a, score_b, score_c):
        x = min(score_a, score_b) - score_c
        if x < 0:   html_tag = "evolutionary-regression"
        elif x > 0: html_tag = "evolutionary-progression"
        else:       html_tag = "evolutionary-stasis"
        x = "%.6f" % x
        return "<{0}>{1}</{0}>".format(html_tag, x)

    @staticmethod
    def join(genotype):
        return '&nbsp'.join(genotype)

    GENE_STATE_TO_HTML_TAG = {
        GeneState.On:       "gene-state-on",
        GeneState.Off:      "gene-state-off",
        GeneState.Mutant:   "gene-state-mutant",
        }
    @classmethod
    def markup_ancestry(cls, genotype, states):
        def html(g, s):
            return "<{0}>{1}</{0}>".format(
                cls.GENE_STATE_TO_HTML_TAG[s], g)
        return [html(g, s) for g, s in zip(genotype, states)]

    @staticmethod
    def markup_score(score):
        return str(score).ljust(14).replace(' ', '&nbsp;')

    @staticmethod
    def get_states(parent_a, parent_b, child):
        """TODO(jhibberd) Explain"""
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
        return [f(a, b, c) for a,b,c in zip(parent_a, parent_b, child)]

    @staticmethod
    def longest_gene(genotypes):
        return max([max(map(len, g)) for g in genotypes])

    @staticmethod
    def pad_genes(longest_gene, genotype):
        return [g.ljust(longest_gene).replace(' ', '&nbsp;') for g in genotype]

    def load_genotypes(self, generation):
        """
        Format:
            hash,genotype
        """
        d = {}
        for ln in self.open_log('genotype', generation):
            hash_, genotype = ln[:-1].split(',', 1)
            genotype = genotype[1:-1].split(',')
            d[hash_] = genotype
        return d 

    def load_scores(self, generation):
        """
        Format:
            hash,score
        """
        d = {}
        for ln in self.open_log('score', generation):
            hash_, score = ln[:-1].split(',')
            d[hash_] = float(score)
        return d

    def load_evolution(self, generation):
        """
        Format:
            parent_a_hash,parent_b_hash,child_hash
        """
        xs = []
        for ln in self.open_log('evolve', generation):
            triple = ln[:-1].split(',')
            xs.append(triple)
        return xs

    def open_log(self, file_type, generation):
        """Syntactic shortcut for getting handle to log file."""
        return open('/tmp/beagle/%s.%s' % (generation, file_type))


application = tornado.web.Application([
    (r"/", MainHandler),
], debug=True, template_path='html')

if __name__ == "__main__":
    application.listen(1831)
    tornado.ioloop.IOLoop.instance().start()

