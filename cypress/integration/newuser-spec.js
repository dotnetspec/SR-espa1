
/// <reference types="cypress" />

describe('New User Home Page', () => {
    context('Actions', () => {
        beforeEach(() => {
            cy.visit('/')
            cy.wait(150)
            //cy.get('#createnewrankingbtn > .s').click()
            //cy.wait(150)
        })

        it('successfully loads create new user page', () => {
            cy.wait(150)
            cy.get('#createnewrankingbtn > .s').click()
            cy.contains('Create New User')
            cy.contains('Please Enter Your User Details And Click \'Register\' below:')
        })

        it('successfully validates username', () => {
            cy.wait(150)
            cy.get('#createnewrankingbtn > .s').click()
            cy.get('#userDetails')
                .type('jonnahb').should('have.value', 'jonnahb')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('jonnahb', { delay: 100 })
                .should('have.value', 'jonnahb')
        })

        it('successfully validates description', () => {
            cy.wait(150)
            cy.get('#createnewrankingbtn > .s').click()
            cy.get('#userDescription')
                .type('jonnahb').should('have.value', 'jonnahb')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('jonnahb', { delay: 100 })
                .should('have.value', 'jonnahb')
        })

        it('successfully validates email', () => {
            cy.wait(150)
            cy.get('#createnewrankingbtn > .s').click()
            cy.get('#userEmail')

                .type('fake@email.com').should('have.value', 'fake@email.com')

                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('slow.typing@email.com', { delay: 100 })
                .should('have.value', 'slow.typing@email.com')
        })

        it('successfully validates mobile number', () => {
            cy.wait(150)
            cy.get('#createnewrankingbtn > .s').click()
            cy.get('#userMobile')
                .clear()
                .type('1234567890').should('have.value', '1234567890')
                .clear()
                // .type() with special character sequences
                .type('{leftarrow}{rightarrow}{uparrow}{downarrow}')
                .type('{del}{selectall}{backspace}')

                // .type() with key modifiers
                .type('{alt}{option}') //these are equivalent
                .type('{ctrl}{control}') //these are equivalent
                .type('{meta}{command}{cmd}') //these are equivalent
                .type('{shift}')

                // Delay each keypress by 0.1 sec
                .type('1234567890', { delay: 100 })
                .should('have.value', '1234567890')
        })
    })
})