"""A library for programmatic software modification

See: https://grammatech.github.io/sel
"""
from setuptools import find_packages, setup

setup(
    name='sel',
    version='0.1.3.dev0',
    description='A library for programmatic software modification',
    url='https://github.com/grammatech/sel',
    author='Eric Schulte and GrammaTech',
    author_email='sel@grammatech.com',
    license='GPLv3+',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Topic :: Software Engineering :: Program Refactoring and Synthesis',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
    ],
    keywords='software-engineering, source, program-synthesis',
    packages=find_packages(),
    package_data={'sel': ['tree-sitter-interface']},
    setup_requires=['wheel'],
    python_requires='>=3.6',
    project_urls={
        'Bug Reports': 'https://github.com/grammatech/sel/issues',
        'Source': 'https://github.com/grammatech/sel/',
    },
)
