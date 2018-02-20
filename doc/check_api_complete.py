#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import os.path
import glob
import re


def convert_eltname(instr):
    outstr = instr
    for old,new in (('*','star'),
                    ('+','plus'),
                    ('&gt;','gt')):
        if old not in outstr:
            continue
        spl = outstr.split(old)
        if spl[-1] == '':
            spl = spl[:-2] + [spl[-2] + '-' + new]
        outstr = (new+'-').join(spl)

    if outstr.startswith('setf '):
        outstr = 'setf-'+outstr[5:-1]
    return outstr

        
# get list of generated documentation files
apiflist = glob.glob(os.path.join('include','*.texinfo'))

elts = {'class':'Class',
        'condition':'Condition',
        'constant':'Constant',
        'fun':'Function',
        'macro':'Macro',
        'struct':'Structure',
        'var':'Variable'}

apis = ((r'^sel-slash-utility-','SEL-Utility-API.html'),
        (r'^sel-slash-view-','SEL-View-API.html'),
        (r'^sel(?!-slash)-', 'SEL-API.html'), )


# catalog every generated lisp documentation file

apiflist = filter(lambda s: s.count('-')>=2,
                  apiflist)

apiflist = map(lambda s: s[len('include/'):-len('.texinfo')],
               apiflist)

                  
file_catalog = dict(
    [(title, ['-'.join(f.split('-')[1:])
              for f in apiflist
              if f.split('-')[0]==pref])
     for (pref,title) in elts.iteritems() ] )

full_file_catalog = dict (
    [(fname, dict( [(title,
                     set([f[len(re.search(pref,f).group(0)):]
                          for f in file_catalog[title]
                          if re.search(pref,f) is not None]))
                    for title in elts.values()]) )
     for pref,fname in apis] )

#print full_file_catalog


# catalog every element that appears in an API documentation page


eltre = r'(</a>|\s+)(?P<elt>{0}): <strong>\s*(\(|)(?P<name>(setf |)[^\s^<]+)'.format('|'.join(elts.values()))

full_doc_catalog = {} 
                          
for docpage in zip(*apis)[1] :
    subdict = dict([(title,set([]))
                                      for title in elts.values()])
    with open(os.path.join('software-evolution-library',docpage)) as f:
        for line in f:
            mo = re.search(eltre,line)
            if mo is not None:
                subdict[mo.group('elt')].add(convert_eltname(mo.group('name')))
            
    full_doc_catalog[docpage] = subdict

#print full_doc_catalog    

# compare the two catalogs

indt = 5*' '
for api in zip(*apis)[1]:
    print '='*50, '\n Checking ',api,'\n', '='*50
    api_file_catalog = full_file_catalog[api]
    api_doc_catalog = full_doc_catalog[api]
       
    matches = [e for e in elts.values()
               if (api_file_catalog[e] == api_doc_catalog[e])
               or (not api_file_catalog[e] and not api_doc_catalog[e])]

    print indt, ('OK: {0}'.format(', '.join(matches)) if matches else '')

    for bad in filter(lambda e: e not in matches,
                      elts.values()):

        print indt, 'BAD', bad

        diff = api_file_catalog[bad] - api_doc_catalog[bad]
        if diff:
            print indt*2, 'File but not in manual'
            print indt*3 + ('\n'+indt*3).join(diff)

        diff = api_doc_catalog[bad] - api_file_catalog[bad]
        if diff:            
            # shouldn't happen: manual should not build
            print indt*2, 'Manual but no file - something is very weird'
            print indt*3 + ('\n'+indt*3).join(diff)
        
#        print 'File list', ', '.join(['|{0}|'.format(k) for k in api_file_catalog[bad]])
#        print 'Manual list', ', '.join(['|{0}|'.format(k) for k in api_doc_catalog[bad]])        

