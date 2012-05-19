import tornado.ioloop
import tornado.web

class GeneAncestry(object):
    """Ancestry of a gene within a genotype (used for color-coding)."""
    ParentA =   0
    ParentB =   1
    Mutation =  2


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

            longest_gene = self.longest_gene([
                genotype_a, genotype_b, genotype_c])

            # TODO(jhibberd) For efficiency add padding after calculating ancestry.
            genotype_a = self.pad_genes(longest_gene, genotype_a)
            genotype_b = self.pad_genes(longest_gene, genotype_b)
            genotype_c = self.pad_genes(longest_gene, genotype_c)

            genotype_c = self.mark_ancestry_diff(genotype_c, genotype_a, genotype_b) 
            genotype_a = self.mark_ancestry_all(genotype_a, GeneAncestry.ParentA)
            genotype_b = self.mark_ancestry_all(genotype_b, GeneAncestry.ParentB)

            genotype_a = self.markup_ancestry(genotype_a) 
            genotype_b = self.markup_ancestry(genotype_b) 
            genotype_c = self.markup_ancestry(genotype_c) 

            # TODO(jhibberd) Join genotypes before rendering.
            # TODO(jhibberd) Choose better colors.

            output.append(
                ((genotype_a, score_a), 
                 (genotype_b, score_b),
                 (genotype_c, score_c)))

        # Display all genotypes in the generation.
        self.render('generation.html', 
            content=output,
            generation=generation,
            )
    
    # TODO(jhibberd) Put these functions into separate classes.

    GENE_ANCESTRY_TO_CSS = {
        GeneAncestry.ParentA:   "gene-type-parent-a",
        GeneAncestry.ParentB:   "gene-type-parent-b",
        GeneAncestry.Mutation:  "gene-type-mutation",
        }
    @classmethod
    def markup_ancestry(cls, genotypes):
        def f(gene, ancestry):
            return "<{0}>{1}</{0}>".format(
                cls.GENE_ANCESTRY_TO_CSS[ancestry], gene)
        return [f(g, a) for g, a in genotypes]

    @staticmethod
    def mark_ancestry_all(genotype, ancestry):
        return map(lambda x: (x, ancestry), genotype)

    @staticmethod
    def mark_ancestry_diff(genotype, parent_a, parent_b):
        def ancestry(gene, a, b):
            if gene == a: return GeneAncestry.ParentA
            if gene == b: return GeneAncestry.ParentB
            return GeneAncestry.Mutation
        return [(g, ancestry(g, a, b)) for g, a, b in \
            zip(genotype, parent_a, parent_b)]

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
            d[hash_] = score
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

