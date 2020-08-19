"""
Model exported as python.
Name : Generate VTISM Table
Group : 
With QGIS : 31200
"""

from qgis.core import QgsProcessing
from qgis.core import QgsProcessingAlgorithm
from qgis.core import QgsProcessingMultiStepFeedback
from qgis.core import QgsProcessingParameterString
import processing


class GenerateVtismTable(QgsProcessingAlgorithm):

    def initAlgorithm(self, config=None):
        self.addParameter(QgsProcessingParameterString('ELRs', 'ELRs', multiLine=False, defaultValue=''))
        self.addParameter(QgsProcessingParameterString('TIDs', 'TIDs', multiLine=False, defaultValue=''))

    def processAlgorithm(self, parameters, context, model_feedback):
        # Use a multi-step feedback, so that individual child algorithm progress reports are adjusted for the
        # overall progress through the model
        feedback = QgsProcessingMultiStepFeedback(1, model_feedback)
        results = {}
        outputs = {}
        
        elrs = self.parameterAsString(parameters, 'elrs', context)
        tids = self.parameterAsString(parameters, 'tids', context)

        # PostgreSQL execute and load SQL
        alg_params = {
            'DATABASE': 'nas-vm1',
            'GEOMETRY_FIELD': 'geom',
            'ID_FIELD': 'ID',
            'SQL': 'SELECT \nST_GEOMFROMEWKT(\'SRID=4326;POINT(0 0)\') as geom,\n*\nFROM analysis.track_2020 \nWHERE\n\"ELR\" IN (select unnest(string_to_array(trim(\'' + elrs
            + '\'), \',\'))) AND substr("TID", 1,2) IN (select unnest(string_to_array(trim(\'' + tids + '\'), \',\')))'
        }
        outputs['PostgresqlExecuteAndLoadSql'] = processing.run('qgis:postgisexecuteandloadsql', alg_params, context=context, feedback=feedback, is_child_algorithm=True)
        return results

    def name(self):
        return 'Generate VTISM Table'

    def displayName(self):
        return 'Generate VTISM Table'

    def group(self):
        return ''

    def groupId(self):
        return ''

    def createInstance(self):
        return GenerateVtismTable()
