# Project Knowledge Graph Example

## Knowledge Graph Metadata
**Title:** Project Management Knowledge Graph  
**Purpose:** To represent the complex relationships between project components, stakeholders, resources, and outcomes  
**Last Updated:** 2025-05-23  
**Curator:** MOAL 2.0 Knowledge Base Team  

## Knowledge Graph Structure

This knowledge graph represents the domain of project management, including projects, tasks, resources, stakeholders, risks, and their interrelationships. It is structured using a formal ontology with defined entity types and relationship types.

### Entity Types

1. **Project**: Temporary endeavors with defined objectives and constraints
2. **Task**: Discrete units of work within a project
3. **Milestone**: Significant points or events in a project
4. **Resource**: Assets used in project execution
5. **Stakeholder**: Individuals or groups with interest in the project
6. **Risk**: Potential events that could impact project outcomes
7. **Constraint**: Limitations on project execution
8. **Deliverable**: Outputs produced by the project
9. **Status**: Current state of project components
10. **Method**: Approaches to project management

### Relationship Types

1. **contains**: Indicates composition relationships
2. **requires**: Shows dependency relationships
3. **allocates**: Indicates resource assignment
4. **influences**: Shows impact relationships
5. **mitigates**: Indicates risk reduction
6. **reports**: Shows reporting relationships
7. **precedes**: Indicates sequential relationships
8. **achieves**: Shows accomplishment relationships
9. **constrains**: Indicates limitation relationships
10. **implements**: Shows application relationships

### Entity Instances

#### Project Entities
- **WebsiteRedesign** [attributes: {startDate: "2025-06-01", endDate: "2025-09-30", budget: 150000, priority: "high"}]
- **ProductLaunch** [attributes: {startDate: "2025-07-15", endDate: "2025-12-15", budget: 300000, priority: "critical"}]
- **OfficeRelocation** [attributes: {startDate: "2025-08-01", endDate: "2025-10-31", budget: 200000, priority: "medium"}]
- **SystemUpgrade** [attributes: {startDate: "2025-09-01", endDate: "2025-11-30", budget: 175000, priority: "high"}]

#### Task Entities
- **RequirementsGathering** [attributes: {duration: "2 weeks", effort: "high", complexity: "medium"}]
- **DesignPhase** [attributes: {duration: "3 weeks", effort: "high", complexity: "high"}]
- **Development** [attributes: {duration: "6 weeks", effort: "high", complexity: "high"}]
- **Testing** [attributes: {duration: "3 weeks", effort: "medium", complexity: "medium"}]
- **Deployment** [attributes: {duration: "1 week", effort: "high", complexity: "high"}]
- **MarketResearch** [attributes: {duration: "4 weeks", effort: "medium", complexity: "medium"}]
- **MarketingCampaign** [attributes: {duration: "8 weeks", effort: "high", complexity: "medium"}]
- **VendorSelection** [attributes: {duration: "3 weeks", effort: "medium", complexity: "low"}]
- **EquipmentProcurement** [attributes: {duration: "4 weeks", effort: "medium", complexity: "low"}]
- **StaffTraining** [attributes: {duration: "2 weeks", effort: "medium", complexity: "medium"}]

#### Milestone Entities
- **RequirementsApproval** [attributes: {date: "2025-06-15", significance: "high"}]
- **DesignSignoff** [attributes: {date: "2025-07-07", significance: "high"}]
- **BetaRelease** [attributes: {date: "2025-08-15", significance: "medium"}]
- **FinalLaunch** [attributes: {date: "2025-09-30", significance: "critical"}]
- **MarketingPlanApproval** [attributes: {date: "2025-08-01", significance: "high"}]
- **ProductionReady** [attributes: {date: "2025-11-01", significance: "critical"}]
- **LocationSelected** [attributes: {date: "2025-08-31", significance: "high"}]
- **MoveCompleted** [attributes: {date: "2025-10-31", significance: "critical"}]

#### Resource Entities
- **DevelopmentTeam** [attributes: {type: "human", availability: "limited", cost: "high"}]
- **DesignTeam** [attributes: {type: "human", availability: "limited", cost: "high"}]
- **MarketingTeam** [attributes: {type: "human", availability: "limited", cost: "medium"}]
- **ProjectBudget** [attributes: {type: "financial", availability: "fixed", cost: "n/a"}]
- **ServerInfrastructure** [attributes: {type: "technical", availability: "scalable", cost: "medium"}]
- **OfficeSpace** [attributes: {type: "physical", availability: "limited", cost: "high"}]
- **ConsultingServices** [attributes: {type: "service", availability: "scheduled", cost: "high"}]
- **SoftwareLicenses** [attributes: {type: "digital", availability: "purchasable", cost: "medium"}]

#### Stakeholder Entities
- **ExecutiveTeam** [attributes: {influence: "high", interest: "medium", internal: true}]
- **EndUsers** [attributes: {influence: "medium", interest: "high", internal: true}]
- **ClientOrganization** [attributes: {influence: "high", interest: "high", internal: false}]
- **RegulatoryBody** [attributes: {influence: "high", interest: "low", internal: false}]
- **ITDepartment** [attributes: {influence: "medium", interest: "high", internal: true}]
- **MarketingDepartment** [attributes: {influence: "medium", interest: "high", internal: true}]
- **Vendors** [attributes: {influence: "low", interest: "medium", internal: false}]
- **Employees** [attributes: {influence: "medium", interest: "high", internal: true}]

#### Risk Entities
- **ScheduleDelay** [attributes: {probability: "medium", impact: "high", category: "time"}]
- **BudgetOverrun** [attributes: {probability: "medium", impact: "high", category: "cost"}]
- **ScopeCreep** [attributes: {probability: "high", impact: "medium", category: "scope"}]
- **ResourceShortage** [attributes: {probability: "medium", impact: "high", category: "resource"}]
- **TechnicalFailure** [attributes: {probability: "low", impact: "critical", category: "technical"}]
- **StakeholderResistance** [attributes: {probability: "medium", impact: "high", category: "organizational"}]
- **VendorDelay** [attributes: {probability: "medium", impact: "medium", category: "external"}]
- **RegulatoryIssues** [attributes: {probability: "low", impact: "high", category: "compliance"}]

#### Constraint Entities
- **Timeline** [attributes: {flexibility: "low", severity: "high"}]
- **Budget** [attributes: {flexibility: "medium", severity: "high"}]
- **Scope** [attributes: {flexibility: "medium", severity: "medium"}]
- **Resources** [attributes: {flexibility: "low", severity: "high"}]
- **TechnicalLimitations** [attributes: {flexibility: "low", severity: "medium"}]
- **RegulatoryRequirements** [attributes: {flexibility: "none", severity: "high"}]
- **StakeholderExpectations** [attributes: {flexibility: "medium", severity: "high"}]
- **OrganizationalPolicies** [attributes: {flexibility: "low", severity: "medium"}]

#### Deliverable Entities
- **WebsiteDesign** [attributes: {tangibility: "digital", complexity: "high"}]
- **FunctionalWebsite** [attributes: {tangibility: "digital", complexity: "high"}]
- **MarketingMaterials** [attributes: {tangibility: "mixed", complexity: "medium"}]
- **ProductPrototype** [attributes: {tangibility: "physical", complexity: "high"}]
- **TrainingDocumentation** [attributes: {tangibility: "digital", complexity: "medium"}]
- **SystemArchitecture** [attributes: {tangibility: "digital", complexity: "high"}]
- **FinalReport** [attributes: {tangibility: "digital", complexity: "low"}]
- **OperationalFacility** [attributes: {tangibility: "physical", complexity: "high"}]

#### Status Entities
- **NotStarted** [attributes: {completion: 0, phase: "planning"}]
- **InProgress** [attributes: {completion: 0.5, phase: "execution"}]
- **AtRisk** [attributes: {completion: 0.3, phase: "execution"}]
- **OnHold** [attributes: {completion: 0.2, phase: "paused"}]
- **Completed** [attributes: {completion: 1.0, phase: "closed"}]
- **Cancelled** [attributes: {completion: 0, phase: "terminated"}]

#### Method Entities
- **Agile** [attributes: {flexibility: "high", iterative: true}]
- **Waterfall** [attributes: {flexibility: "low", iterative: false}]
- **Scrum** [attributes: {flexibility: "high", iterative: true}]
- **Kanban** [attributes: {flexibility: "high", iterative: true}]
- **PRINCE2** [attributes: {flexibility: "medium", iterative: false}]
- **LeanPM** [attributes: {flexibility: "high", iterative: true}]

### Relationship Instances

#### Project Structure Relationships
- **WebsiteRedesign** → *contains* → **RequirementsGathering**
- **WebsiteRedesign** → *contains* → **DesignPhase**
- **WebsiteRedesign** → *contains* → **Development**
- **WebsiteRedesign** → *contains* → **Testing**
- **WebsiteRedesign** → *contains* → **Deployment**
- **WebsiteRedesign** → *achieves* → **RequirementsApproval**
- **WebsiteRedesign** → *achieves* → **DesignSignoff**
- **WebsiteRedesign** → *achieves* → **BetaRelease**
- **WebsiteRedesign** → *achieves* → **FinalLaunch**
- **WebsiteRedesign** → *implements* → **Agile**

- **ProductLaunch** → *contains* → **MarketResearch**
- **ProductLaunch** → *contains* → **Development**
- **ProductLaunch** → *contains* → **MarketingCampaign**
- **ProductLaunch** → *contains* → **Testing**
- **ProductLaunch** → *achieves* → **MarketingPlanApproval**
- **ProductLaunch** → *achieves* → **ProductionReady**
- **ProductLaunch** → *implements* → **Waterfall**

- **OfficeRelocation** → *contains* → **VendorSelection**
- **OfficeRelocation** → *contains* → **EquipmentProcurement**
- **OfficeRelocation** → *achieves* → **LocationSelected**
- **OfficeRelocation** → *achieves* → **MoveCompleted**
- **OfficeRelocation** → *implements* → **PRINCE2**

- **SystemUpgrade** → *contains* → **RequirementsGathering**
- **SystemUpgrade** → *contains* → **Development**
- **SystemUpgrade** → *contains* → **Testing**
- **SystemUpgrade** → *contains* → **StaffTraining**
- **SystemUpgrade** → *implements* → **Agile**

#### Task Dependencies
- **RequirementsGathering** → *precedes* → **DesignPhase**
- **DesignPhase** → *precedes* → **Development**
- **Development** → *precedes* → **Testing**
- **Testing** → *precedes* → **Deployment**
- **MarketResearch** → *precedes* → **MarketingCampaign**
- **VendorSelection** → *precedes* → **EquipmentProcurement**

#### Resource Allocations
- **WebsiteRedesign** → *allocates* → **DevelopmentTeam**
- **WebsiteRedesign** → *allocates* → **DesignTeam**
- **WebsiteRedesign** → *allocates* → **ProjectBudget**
- **WebsiteRedesign** → *allocates* → **ServerInfrastructure**

- **ProductLaunch** → *allocates* → **MarketingTeam**
- **ProductLaunch** → *allocates* → **DevelopmentTeam**
- **ProductLaunch** → *allocates* → **ProjectBudget**
- **ProductLaunch** → *allocates* → **ConsultingServices**

- **OfficeRelocation** → *allocates* → **ProjectBudget**
- **OfficeRelocation** → *allocates* → **OfficeSpace**
- **OfficeRelocation** → *allocates* → **ConsultingServices**

- **SystemUpgrade** → *allocates* → **DevelopmentTeam**
- **SystemUpgrade** → *allocates* → **ProjectBudget**
- **SystemUpgrade** → *allocates* → **ServerInfrastructure**
- **SystemUpgrade** → *allocates* → **SoftwareLicenses**

#### Stakeholder Relationships
- **WebsiteRedesign** → *reports* → **ExecutiveTeam**
- **WebsiteRedesign** → *reports* → **ClientOrganization**
- **WebsiteRedesign** → *influences* → **EndUsers**
- **WebsiteRedesign** → *influences* → **ITDepartment**

- **ProductLaunch** → *reports* → **ExecutiveTeam**
- **ProductLaunch** → *influences* → **MarketingDepartment**
- **ProductLaunch** → *influences* → **EndUsers**
- **ProductLaunch** → *reports* → **ClientOrganization**

- **OfficeRelocation** → *reports* → **ExecutiveTeam**
- **OfficeRelocation** → *influences* → **Employees**
- **OfficeRelocation** → *influences* → **Vendors**

- **SystemUpgrade** → *reports* → **ExecutiveTeam**
- **SystemUpgrade** → *influences* → **ITDepartment**
- **SystemUpgrade** → *influences* → **EndUsers**
- **SystemUpgrade** → *reports* → **RegulatoryBody**

#### Risk Relationships
- **WebsiteRedesign** → *contains* → **ScheduleDelay**
- **WebsiteRedesign** → *contains* → **ScopeCreep**
- **WebsiteRedesign** → *contains* → **TechnicalFailure**
- **Agile** → *mitigates* → **ScopeCreep**
- **RequirementsApproval** → *mitigates* → **ScopeCreep**

- **ProductLaunch** → *contains* → **BudgetOverrun**
- **ProductLaunch** → *contains* → **MarketingCampaign**
- **ProductLaunch** → *contains* → **StakeholderResistance**
- **MarketResearch** → *mitigates* → **StakeholderResistance**

- **OfficeRelocation** → *contains* → **VendorDelay**
- **OfficeRelocation** → *contains* → **BudgetOverrun**
- **VendorSelection** → *mitigates* → **VendorDelay**

- **SystemUpgrade** → *contains* → **TechnicalFailure**
- **SystemUpgrade** → *contains* → **RegulatoryIssues**
- **SystemUpgrade** → *contains* → **ResourceShortage**
- **StaffTraining** → *mitigates* → **TechnicalFailure**

#### Constraint Relationships
- **Timeline** → *constrains* → **WebsiteRedesign**
- **Budget** → *constrains* → **WebsiteRedesign**
- **TechnicalLimitations** → *constrains* → **WebsiteRedesign**

- **StakeholderExpectations** → *constrains* → **ProductLaunch**
- **Timeline** → *constrains* → **ProductLaunch**
- **Budget** → *constrains* → **ProductLaunch**

- **OrganizationalPolicies** → *constrains* → **OfficeRelocation**
- **Budget** → *constrains* → **OfficeRelocation**

- **RegulatoryRequirements** → *constrains* → **SystemUpgrade**
- **Resources** → *constrains* → **SystemUpgrade**
- **TechnicalLimitations** → *constrains* → **SystemUpgrade**

#### Deliverable Relationships
- **WebsiteRedesign** → *produces* → **WebsiteDesign**
- **WebsiteRedesign** → *produces* → **FunctionalWebsite**
- **WebsiteRedesign** → *produces* → **FinalReport**

- **ProductLaunch** → *produces* → **MarketingMaterials**
- **ProductLaunch** → *produces* → **ProductPrototype**
- **ProductLaunch** → *produces* → **FinalReport**

- **OfficeRelocation** → *produces* → **OperationalFacility**
- **OfficeRelocation** → *produces* → **FinalReport**

- **SystemUpgrade** → *produces* → **SystemArchitecture**
- **SystemUpgrade** → *produces* → **TrainingDocumentation**
- **SystemUpgrade** → *produces* → **FinalReport**

#### Status Relationships
- **RequirementsGathering** → *reports* → **Completed**
- **DesignPhase** → *reports* → **InProgress**
- **Development** → *reports* → **NotStarted**
- **Testing** → *reports* → **NotStarted**
- **Deployment** → *reports* → **NotStarted**

- **MarketResearch** → *reports* → **InProgress**
- **MarketingCampaign** → *reports* → **NotStarted**

- **VendorSelection** → *reports* → **AtRisk**
- **EquipmentProcurement** → *reports* → **OnHold**

- **RequirementsGathering** → *reports* → **Completed**
- **Development** → *reports* → **NotStarted**
- **Testing** → *reports* → **NotStarted**
- **StaffTraining** → *reports* → **NotStarted**

### Complex Queries This Knowledge Graph Can Answer

1. **Project Risk Assessment**:
   - "What are the highest impact risks across all projects?"
   - "Which projects have the most unmitigated risks?"
   - "What mitigation strategies are in place for schedule delays?"

2. **Resource Allocation Analysis**:
   - "Which resources are allocated across multiple projects simultaneously?"
   - "What is the total budget allocation across all active projects?"
   - "Which projects are competing for the same limited resources?"

3. **Critical Path Identification**:
   - "What is the sequence of dependent tasks that determines the minimum project timeline?"
   - "Which tasks, if delayed, would impact the overall project completion date?"
   - "What are the dependencies between milestones across projects?"

4. **Stakeholder Impact Assessment**:
   - "Which stakeholders are influenced by multiple projects?"
   - "What projects require regulatory body reporting?"
   - "Which stakeholders have high influence but low interest in projects?"

5. **Project Health Dashboard**:
   - "What is the status of all deliverables across projects?"
   - "Which projects have tasks that are at risk or on hold?"
   - "What is the overall completion percentage of each project?"

## Usage Notes
- This knowledge graph represents the complex domain of project management with formal entity and relationship types
- It can be used to:
  - Track project status and dependencies
  - Analyze resource allocation across multiple projects
  - Identify and mitigate risks
  - Understand stakeholder relationships and impacts
  - Monitor deliverables and milestones
- The graph can be extended with additional entities, attributes, and relationships
- Consider implementing this in a graph database for efficient querying and visualization
- This representation supports both project management and portfolio management activities
