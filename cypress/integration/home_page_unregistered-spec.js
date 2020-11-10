
/// <reference types="cypress" />

const users = require('../fixtures/users.json')
const rankings = require('../fixtures/globalRankings.json')

describe('Unregistered User', () => {
    context('Actions', () => {
        beforeEach(() => {
            cy.visit('/')
            //cy.server()
            //cy.fixture('users').then((json) => {
            //cy.route('GET', '/users/**', json)
            //cy.route('GET', 'https://localhost:3000', json)
        })

        it('successfully confirms page header, moves to a ranking, selects Join button', () => {
            cy.contains('SportRank - New User')
            cy.contains('Register')
            cy.contains('View Rankings:')
            cy.get(':nth-child(1) > .cptr').click()
            cy.contains('SportRank - New User - Join?')
            cy.contains('Join')
            cy.contains('Selected Ranking')
            //cy.get('#newUserJoinbtn').click()
            // cy.contains('Create New User')
        })

        // it.skip('successfully loads a ranking', () => {
        //     cy.contains('SportRank - New User')
        //     cy.contains('Register')
        //     cy.contains('View Rankings:')
        //     cy.get(':nth-child(1) > .cptr').click()
        //     // cy.contains('SportRank - New User - Join?')
        //     cy.contains('Join')
        //     cy.contains('Selected Ranking')
        // })
    })
})
        //attempts to get json from fixtures fail currently
        // it('has user', () => {
        //     expect(users).to.deep.equal({ username: 'Test 10' })
        // })

        // it.skip('has ranking', () => {
        //     expect(rankings).to.deep.equal({ id: '5e9a96572940c704e1da6f9f' })
        // })










