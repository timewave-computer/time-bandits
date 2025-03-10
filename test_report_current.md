
| Test Mode Scenarios | In-Memory Mode Resource Transfer Scenario | Tests resource transfer in In-Memory mode | ✅ |
| Test Mode Scenarios | Local Multi-Process Mode Resource Transfer Scenario | Tests resource transfer in Local Multi-Process mode | ✅ |
| Test Mode Scenarios | Geo-Distributed Mode Simulation | Tests environment setup for Geo-Distributed mode | ✅ |

## 📝 Test Mode Scenario Tests

The Test Mode Scenario Tests validate that the Time Bandits system can correctly function in all supported execution modes:

### In-Memory Mode
- All actors run in a single process, directly invoking each other's functions
- Tests resource transfer between timelines

### Local Multi-Process Mode  
- Each actor runs in its own separate process, started by the simulation controller
- Tests the same resource transfer scenario as in-memory mode

### Geo-Distributed Mode
- Setup for actors running on remote machines
- Tests environment configuration for distributed deployment

These tests ensure that the same business logic works correctly across all deployment modes, from development to production environments.
